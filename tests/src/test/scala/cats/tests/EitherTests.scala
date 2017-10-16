package cats
package tests

import cats.data.{ EitherT, Validated }
import cats.laws.discipline._
import cats.kernel.laws.discipline.{
  MonoidTests => MonoidLawTests,
  SemigroupTests => SemigroupLawTests,
  OrderTests => OrderLawTests,
  PartialOrderTests => PartialOrderLawTests,
  EqTests => EqLawTests
}
import scala.util.Try

class EitherTests extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Either[Int, ?]]

  checkAll("Either[String, Int]", MonoidLawTests[Either[String, Int]].monoid)
  checkAll("Monoid[Either[String, Int]]", SerializableTests.serializable(Monoid[Either[String, Int]]))

  checkAll("Either[Int, Int]", SemigroupalTests[Either[Int, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Either[Int, ?]]", SerializableTests.serializable(Semigroupal[Either[Int, ?]]))

  implicit val eq0 = EitherT.catsDataEqForEitherT[Either[Int, ?], Int, Int]

  checkAll("Either[Int, Int]", MonadErrorTests[Either[Int, ?], Int].monadError[Int, Int, Int])
  checkAll("MonadError[Either[Int, ?]]", SerializableTests.serializable(MonadError[Either[Int, ?], Int]))

  checkAll("Either[Int, Int] with Option", TraverseTests[Either[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Either[Int, ?]", SerializableTests.serializable(Traverse[Either[Int, ?]]))

  checkAll("Either[?, ?]", BitraverseTests[Either].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Either]", SerializableTests.serializable(Bitraverse[Either]))

  checkAll("Either[ListWrapper[String], ?]", SemigroupKTests[Either[ListWrapper[String], ?]].semigroupK[Int])
  checkAll("SemigroupK[Either[ListWrapper[String], ?]]", SerializableTests.serializable(SemigroupK[Either[ListWrapper[String], ?]]))

  checkAll("Either[ListWrapper[String], Int]", SemigroupLawTests[Either[ListWrapper[String], Int]].semigroup)
  checkAll("Semigroup[Either[ListWrapper[String], Int]]", SerializableTests.serializable(Semigroup[Either[ListWrapper[String], Int]]))

  val partialOrder = catsStdPartialOrderForEither[Int, String]
  val order = implicitly[Order[Either[Int, String]]]
  val monad = implicitly[Monad[Either[Int, ?]]]
  val show = implicitly[Show[Either[Int, String]]]

  {
    implicit val S = ListWrapper.eqv[String]
    implicit val I = ListWrapper.eqv[Int]
    checkAll("Either[ListWrapper[String], ListWrapper[Int]]", EqLawTests[Either[ListWrapper[String], ListWrapper[Int]]].eqv)
    checkAll("Eq[Either[ListWrapper[String], ListWrapper[Int]]]", SerializableTests.serializable(Eq[Either[ListWrapper[String], ListWrapper[Int]]]))
  }

  checkAll("Either[Int, String]", PartialOrderLawTests[Either[Int, String]](partialOrder).partialOrder)
  checkAll("Either[Int, String]", OrderLawTests[Either[Int, String]](order).order)

  test("Left/Right cast syntax") {
    forAll { (e: Either[Int, String]) =>
      e match {
        case l @ Left(_)  =>
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
    val eq = catsStdEqForEither[Int, String]
    assert(!eq.isInstanceOf[PartialOrder[_]])
    assert(!eq.isInstanceOf[Order[_]])
    assert(!partialOrder.isInstanceOf[Order[_]])
  }

  test("show isn't empty") {
    forAll { (e: Either[Int, String]) =>
      show.show(e).nonEmpty should === (true)
    }
  }

  test("map2Eval is lazy") {
    val bomb: Eval[Either[String, Int]] = Later(sys.error("boom"))
    val x: Either[String, Int] = Left("l")
    x.map2Eval(bomb)(_ + _).value should === (x)
  }

  test("catchOnly lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Either.catchOnly[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  test("catchNonFatal catches non-fatal exceptions") {
    assert(Either.catchNonFatal{ "foo".toInt }.isLeft)
    assert(Either.catchNonFatal{ throw new Throwable("blargh") }.isLeft)
  }

  test("fromTry is left for failed Try") {
    forAll { t: Try[Int] =>
      t.isFailure should === (Either.fromTry(t).isLeft)
    }
  }

  test("fromOption isLeft consistent with Option.isEmpty") {
    forAll { (o: Option[Int], s: String) =>
      Either.fromOption(o, s).isLeft should === (o.isEmpty)
    }
  }

  test("double swap is identity") {
    forAll { (x: Either[Int, String]) =>
      x.swap.swap should === (x)
    }
  }

  test("swap negates isLeft/isRight") {
    forAll { (x: Either[Int, String]) =>
      x.isLeft should !== (x.swap.isLeft)
      x.isRight should !== (x.swap.isRight)
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: Either[Int, String]) =>
      x.isLeft should !== (x.isRight)
    }
  }

  test("foreach is noop for left") {
    forAll { (x: Either[Int, String]) =>
      var count = 0
      x.foreach{ _ => count += 1}
      (count == 0) should === (x.isLeft)
    }
  }

  test("getOrElse ignores default for right") {
    forAll { (x: Either[Int, String], s: String, t: String) =>
      if (x.isRight) {
        x.getOrElse(s) should === (x.getOrElse(t))
      }
    }
  }

  test("orElse") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      val z = x.orElse(y)
      (z === (x)) || (z === (y)) should === (true)
    }
  }

  test("recover recovers handled values") {
    val either = Either.left[String, Int]("either")
    either.recover { case "either" => 5 }.isRight should === (true)
  }

  test("recover ignores unhandled values") {
    val either = Either.left[String, Int]("either")
    either.recover { case "noteither" => 5 } should === (either)
  }

  test("recover ignores the right side") {
    val either = Either.right[String, Int](10)
    either.recover { case "either" => 5 } should === (either)
  }

  test("recoverWith recovers handled values") {
    val either = Either.left[String, Int]("either")
    either.recoverWith { case "either" => Either.right[String, Int](5) }.isRight should === (true)
  }

  test("recoverWith ignores unhandled values") {
    val either = Either.left[String, Int]("either")
    either.recoverWith { case "noteither" => Either.right[String, Int](5) } should === (either)
  }

  test("recoverWith ignores the right side") {
    val either = Either.right[String, Int](10)
    either.recoverWith { case "either" => Either.right[String, Int](5) } should === (either)
  }

  test("valueOr consistent with swap then map then merge") {
    forAll { (x: Either[Int, String], f: Int => String) =>
      x.valueOr(f) should === (x.swap.map(f).merge)
    }
  }

  test("isLeft implies forall") {
    forAll { (x: Either[Int, String], p: String => Boolean) =>
      if (x.isLeft) {
        x.forall(p) should === (true)
      }
    }
  }

  test("isLeft implies exists is false") {
    forAll { (x: Either[Int, String], p: String => Boolean) =>
      if (x.isLeft) {
        x.exists(p) should === (false)
      }
    }
  }

  test("toIor then toEither is identity") {
    forAll { (x: Either[Int, String]) =>
      x.toIor.toEither should === (x)
    }
  }

  test("toTry then fromTry is identity") {
    implicit def eqTh: Eq[Throwable] = Eq.allEqual

    forAll { (x: Throwable Either String) =>
      Either.fromTry(x.toTry) should === (x)
    }
  }

  test("isLeft consistency") {
    forAll { (x: Either[Int, String]) =>
      x.isLeft should === (x.toOption.isEmpty)
      x.isLeft should === (x.toList.isEmpty)
      x.isLeft should === (x.toValidated.isInvalid)
      x.isLeft should === (x.toValidatedNel.isInvalid)
      Option(x.isLeft) should === (x.toEitherT[Option].isLeft)
    }
  }

  test("withValidated") {
    forAll { (x: Either[Int, String], f: Int => Double) =>
      x.withValidated(_.bimap(f, identity)) should === (x.leftMap(f))
    }
  }

  test("combine is right iff both operands are right") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      x.combine(y).isRight should === (x.isRight && y.isRight)
    }
  }

  test("to consistent with toList") {
    forAll { (x: Either[Int, String]) =>
      x.to[List] should === (x.toList)
    }
  }

  test("to consistent with toOption") {
    forAll { (x: Either[Int, String]) =>
      x.to[Option] should === (x.toOption)
    }
  }

  test("partialCompare consistent with PartialOrder") {
    forAll { (x: Either[Int, String], y: Either[Int, String])  =>
      x.partialCompare(y) should === (partialOrder.partialCompare(x, y))
    }
  }

  test("show Right") {
    val either = Either.right[String, Int](10)
    either.show should === ("Right(10)")
  }

  test("show Left") {
    val either = Either.left[String, Int]("string")
    either.show should === ("Left(string)")
  }

  test("ap consistent with Applicative") {
    val fab = implicitly[Applicative[Either[String, ?]]]
    forAll { (fa: Either[String, Int],
              f: Int => String) =>
      fa.ap(Either.right(f)) should === (fab.map(fa)(f))
    }
  }

  test("raiseOrPure syntax consistent with fromEither") {
    val ev = ApplicativeError[Validated[String, ?], String]
    forAll { (fa: Either[String, Int]) =>
      fa.raiseOrPure[Validated[String, ?]] should === (ev.fromEither(fa))
    }
  }

}
