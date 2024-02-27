/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats._
import cats.data.{EitherT, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector, Validated}
import cats.syntax.bifunctor._
import cats.kernel.laws.discipline.{EqTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.either._

import scala.util.Try
import cats.syntax.eq._
import org.scalacheck.Prop._

class EitherSuite extends CatsSuite {
  implicit val iso: Isomorphisms[Either[Int, *]] = Isomorphisms.invariant[Either[Int, *]]

  checkAll("Either[String, Int]", MonoidTests[Either[String, Int]].monoid)
  checkAll("Monoid[Either[String, Int]]", SerializableTests.serializable(Monoid[Either[String, Int]]))

  checkAll("Either[Int, Int]", SemigroupalTests[Either[Int, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Either[Int, *]]", SerializableTests.serializable(Semigroupal[Either[Int, *]]))

  checkAll("Either[Int, Int]", AlignTests[Either[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[Either[Int, *]]", SerializableTests.serializable(Align[Either[Int, *]]))

  implicit val eq0: Eq[EitherT[Either[Int, *], Int, Int]] = EitherT.catsDataEqForEitherT[Either[Int, *], Int, Int]

  checkAll("Either[Int, Int]", MonadErrorTests[Either[Int, *], Int].monadError[Int, Int, Int])
  checkAll("MonadError[Either[Int, *]]", SerializableTests.serializable(MonadError[Either[Int, *], Int]))

  checkAll("Either[Int, Int] with Option", TraverseTests[Either[Int, *]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Either[Int, *]", SerializableTests.serializable(Traverse[Either[Int, *]]))

  checkAll("Either[*, *]", BitraverseTests[Either].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Either]", SerializableTests.serializable(Bitraverse[Either]))

  checkAll("Either[ListWrapper[String], *]", SemigroupKTests[Either[ListWrapper[String], *]].semigroupK[Int])
  checkAll("SemigroupK[Either[ListWrapper[String], *]]",
           SerializableTests.serializable(SemigroupK[Either[ListWrapper[String], *]])
  )

  checkAll("Either[ListWrapper[String], Int]", SemigroupTests[Either[ListWrapper[String], Int]].semigroup)
  checkAll("Semigroup[Either[ListWrapper[String], Int]]",
           SerializableTests.serializable(Semigroup[Either[ListWrapper[String], Int]])
  )

  val partialOrder = cats.kernel.instances.either.catsStdPartialOrderForEither[Int, String]
  val order = implicitly[Order[Either[Int, String]]]
  val monad = implicitly[Monad[Either[Int, *]]]
  val show = implicitly[Show[Either[Int, String]]]

  {
    implicit val S: Eq[ListWrapper[String]] = ListWrapper.eqv[String]
    implicit val I: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("Either[ListWrapper[String], ListWrapper[Int]]",
             EqTests[Either[ListWrapper[String], ListWrapper[Int]]].eqv
    )
    checkAll("Eq[Either[ListWrapper[String], ListWrapper[Int]]]",
             SerializableTests.serializable(Eq[Either[ListWrapper[String], ListWrapper[Int]]])
    )
  }

  checkAll("Either[Int, String]", PartialOrderTests[Either[Int, String]](partialOrder).partialOrder)
  checkAll("Either[Int, String]", OrderTests[Either[Int, String]](order).order)

  test("Left/Right cast syntax") {
    forAll { (e: Either[Int, String]) =>
      e match {
        case l @ Left(_) =>
          l.rightCast[Double]: Either[Int, Double]
          assert(true)
        case r @ Right(_) =>
          r.leftCast[List[Byte]]: Either[List[Byte], String]
          assert(true)
      }
    }
  }

  test("Left/Right id syntax") {
    forAll { (e: Int) =>
      assert(Left[Int, String](e) === e.asLeft[String])
      assert(Right[String, Int](e) === e.asRight[String])
    }
  }

  test("implicit instances resolve specifically") {
    val eq = cats.kernel.instances.either.catsStdEqForEither[Int, String]
    assert(!eq.isInstanceOf[PartialOrder[?]])
    assert(!eq.isInstanceOf[Order[?]])
    assert(!partialOrder.isInstanceOf[Order[?]])
  }

  test("show isn't empty") {
    forAll { (e: Either[Int, String]) =>
      assert(show.show(e).nonEmpty === true)
    }
  }

  test("map2Eval is lazy") {
    val bomb: Eval[Either[String, Int]] = Later(sys.error("boom"))
    val x: Either[String, Int] = Left("l")
    assert(x.map2Eval(bomb)(_ + _).value === x)
  }

  test("catchOnly lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Either.catchOnly[IndexOutOfBoundsException]("foo".toInt)
    }
  }

  test("catchNonFatal catches non-fatal exceptions") {
    assert(Either.catchNonFatal("foo".toInt).isLeft)
    assert(Either.catchNonFatal(throw new Throwable("blargh")).isLeft)
  }

  test("fromTry is left for failed Try") {
    forAll { (t: Try[Int]) =>
      assert(t.isFailure === (Either.fromTry(t).isLeft))
    }
  }

  test("fromOption isLeft consistent with Option.isEmpty") {
    forAll { (o: Option[Int], s: String) =>
      assert(Either.fromOption(o, s).isLeft === (o.isEmpty))
    }
  }

  test("leftNel is consistent with left(NEL)") {
    forAll { (s: String) =>
      assert(Either.leftNel[String, Int](s) === (Either.left[NonEmptyList[String], Int](NonEmptyList.one(s))))
    }
  }

  test("rightNel is consistent with right") {
    forAll { (i: Int) =>
      assert(Either.rightNel[String, Int](i) === (Either.right[NonEmptyList[String], Int](i)))
    }
  }

  test("double swap is identity") {
    forAll { (x: Either[Int, String]) =>
      assert(x.swap.swap === x)
    }
  }

  test("leftNec is consistent with left(NEC)") {
    forAll { (s: String) =>
      assert(Either.leftNec[String, Int](s) === (Either.left[NonEmptyChain[String], Int](NonEmptyChain.one(s))))
    }
  }
  test("rightNec is consistent with right") {
    forAll { (i: Int) =>
      assert(Either.rightNec[String, Int](i) === (Either.right[NonEmptyChain[String], Int](i)))
    }
  }

  test("leftNes is consistent with left(NES)") {
    forAll { (s: String) =>
      assert(Either.leftNes[String, Int](s) === (Either.left[NonEmptySet[String], Int](NonEmptySet.one(s))))
    }
  }

  test("rightNes is consistent with right") {
    forAll { (i: Int) =>
      assert(Either.rightNes[String, Int](i) === (Either.right[NonEmptySet[String], Int](i)))
    }
  }

  test("swap negates isLeft/isRight") {
    forAll { (x: Either[Int, String]) =>
      assert(x.isLeft =!= x.swap.isLeft)
      assert(x.isRight =!= x.swap.isRight)
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: Either[Int, String]) =>
      assert(x.isLeft =!= x.isRight)
    }
  }

  test("foreach is noop for left") {
    forAll { (x: Either[Int, String]) =>
      var count = 0
      x.foreach { _ =>
        count += 1
      }
      assert((count == 0) === (x.isLeft))
    }
  }

  test("getOrElse ignores default for right") {
    forAll { (x: Either[Int, String], s: String, t: String) =>
      if (x.isRight) {
        assert(x.getOrElse(s) === (x.getOrElse(t)))
      }
    }
  }

  test("orElse") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      val z = x.orElse(y)
      assert((z === x) || (z === y) === true)
    }
  }

  test("recover recovers handled values") {
    val either = Either.left[String, Int]("either")
    assert(either.recover { case "either" => 5 }.isRight === true)
  }

  test("recover ignores unhandled values") {
    val either = Either.left[String, Int]("either")
    assert(either.recover { case "noteither" => 5 } === either)
  }

  test("recover ignores the right side") {
    val either = Either.right[String, Int](10)
    assert(either.recover { case "either" => 5 } === either)
  }

  test("recoverWith recovers handled values") {
    val either = Either.left[String, Int]("either")
    assert(either.recoverWith { case "either" => Either.right[String, Int](5) }.isRight === true)
  }

  test("recoverWith ignores unhandled values") {
    val either = Either.left[String, Int]("either")
    assert(either.recoverWith { case "noteither" => Either.right[String, Int](5) } === either)
  }

  test("recoverWith ignores the right side") {
    val either = Either.right[String, Int](10)
    assert(either.recoverWith { case "either" => Either.right[String, Int](5) } === either)
  }

  test("valueOr consistent with swap then map then merge") {
    forAll { (x: Either[Int, String], f: Int => String) =>
      assert(x.valueOr(f) === (x.swap.map(f).merge))
    }
  }

  test("isLeft implies forall") {
    forAll { (x: Either[Int, String], p: String => Boolean) =>
      if (x.isLeft) {
        assert(x.forall(p) === true)
      }
    }
  }

  test("isLeft implies exists is false") {
    forAll { (x: Either[Int, String], p: String => Boolean) =>
      if (x.isLeft) {
        assert(x.exists(p) === false)
      }
    }
  }

  test("toIor then toEither is identity") {
    forAll { (x: Either[Int, String]) =>
      assert(x.toIor.toEither === x)
    }
  }

  test("toTry then fromTry is identity") {
    implicit def eqTh: Eq[Throwable] = Eq.allEqual

    forAll { (x: Throwable Either String) =>
      assert(Either.fromTry(x.toTry) === x)
    }
  }

  test("isLeft consistency") {
    forAll { (x: Either[Int, String]) =>
      assert(x.isLeft === (x.toOption.isEmpty))
      assert(x.isLeft === (x.toList.isEmpty))
      assert(x.isLeft === (x.toValidated.isInvalid))
      assert(x.isLeft === (x.toValidatedNel.isInvalid))
      assert(x.isLeft === (x.toValidatedNec.isInvalid))
      assert(x.isLeft === (x.leftLiftTo[NonEmptyVector].isLeft))
      assert(Option(x.isLeft) === (x.toEitherT[Option].isLeft))
    }
  }

  test("withValidated") {
    forAll { (x: Either[Int, String], f: Int => Double) =>
      assert(x.withValidated(_.bimap(f, identity)) === (x.leftMap(f)))
    }
  }

  test("combine is right iff both operands are right") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      assert(x.combine(y).isRight === (x.isRight && y.isRight))
    }
  }

  test("to consistent with toList") {
    forAll { (x: Either[Int, String]) =>
      assert(x.to[List] === (x.toList))
    }
  }

  test("to consistent with toOption") {
    forAll { (x: Either[Int, String]) =>
      assert(x.to[Option] === (x.toOption))
    }
  }

  test("partialCompare consistent with PartialOrder") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      assert(x.partialCompare(y) === (partialOrder.partialCompare(x, y)))
    }
  }

  test("toEitherNec Left") {
    val either = Either.left[String, Int]("oops")
    assert(either.toEitherNec === (Either.left[NonEmptyChain[String], Int](NonEmptyChain.one("oops"))))
  }
  test("toEitherNec Right") {
    val either = Either.right[String, Int](42)
    assert(either.toEitherNec === (Either.right[NonEmptyChain[String], Int](42)))
  }

  test("toEitherNes Left") {
    val either = Either.left[String, Int]("oops")
    assert(either.toEitherNes === (Either.left[NonEmptySet[String], Int](NonEmptySet.one("oops"))))
  }
  test("toEitherNes Right") {
    val either = Either.right[String, Int](42)
    assert(either.toEitherNes === (Either.right[NonEmptySet[String], Int](42)))
  }

  test("show Right") {
    val either = Either.right[String, Int](10)
    assert(either.show === "Right(10)")
  }

  test("show Left") {
    val either = Either.left[String, Int]("string")
    assert(either.show === "Left(string)")
  }

  test("toEitherNel Left") {
    val either = Either.left[String, Int]("oops")
    assert(either.toEitherNel === (Either.left[NonEmptyList[String], Int](NonEmptyList.one("oops"))))
  }

  test("toEitherNel Right") {
    val either = Either.right[String, Int](42)
    assert(either.toEitherNel === (Either.right[NonEmptyList[String], Int](42)))
  }

  test("leftLiftTo Left") {
    forAll { (y: String) =>
      assert(
        y.asLeft[Int].leftLiftTo[NonEmptyVector] === Either.left[NonEmptyVector[String], Int](
          NonEmptyVector.one(y)
        )
      )
    }
  }

  test("leftLiftTo Right") {
    forAll { (x: Int) =>
      assert(x.asRight[String].leftLiftTo[NonEmptyVector] === (Either.right[NonEmptyVector[String], Int](x)))
    }
  }

  test("ap consistent with Applicative") {
    val fab = implicitly[Applicative[Either[String, *]]]
    forAll { (fa: Either[String, Int], f: Int => String) =>
      assert(fa.ap(Either.right(f)) === (fab.map(fa)(f)))
    }
  }

  test("liftTo syntax consistent with fromEither") {
    val ev = ApplicativeError[Validated[String, *], String]
    forAll { (fa: Either[String, Int]) =>
      assert(fa.liftTo[Validated[String, *]] === (ev.fromEither(fa)))
    }
  }

  test("leftFlatMap consistent with leftMap") {
    forAll { (either: Either[String, Int], f: String => String) =>
      assert(either.leftFlatMap(v => Left(f(v))) === (either.leftMap(f)))
    }
  }

  test("leftFlatMap consistent with swap and then flatMap") {
    forAll { (either: Either[String, Int], f: String => Either[String, Int]) =>
      assert(either.leftFlatMap(f) === (either.swap.flatMap(a => f(a).swap).swap))
    }
  }

  test("raiseWhen raises when true") {
    val result = Either.raiseWhen(true)("ok")
    assert(result === Left("ok"))
  }

  test("raiseUnless raises when false") {
    val result = Either.raiseUnless(false)("ok")
    assert(result === Left("ok"))
  }
}

final class EitherInstancesSuite extends munit.FunSuite {

  test("parallel instance in cats.instances.either") {
    import cats.instances.either._
    import cats.syntax.parallel._

    def either: Either[String, Int] = Left("Test")
    (either, either).parTupled
  }
}

@deprecated("To test deprecated methods", "2.1.0")
class DeprecatedEitherSuite extends CatsSuite {
  test("raiseOrPure syntax consistent with fromEither") {
    val ev = ApplicativeError[Validated[String, *], String]
    forAll { (fa: Either[String, Int]) =>
      assert(fa.raiseOrPure[Validated[String, *]] === (ev.fromEither(fa)))
    }
  }
}
