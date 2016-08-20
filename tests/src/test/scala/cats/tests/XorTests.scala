package cats
package tests

import cats.data.{NonEmptyList, Xor, XorT}
import cats.data.Xor._
import cats.laws.discipline.{SemigroupKTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{BitraverseTests, TraverseTests, MonadErrorTests, MonadTests, SerializableTests, CartesianTests}
import cats.kernel.laws.{GroupLaws, OrderLaws}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import scala.util.Try

class XorTests extends CatsSuite {
  checkAll("Xor[String, Int]", GroupLaws[Xor[String, Int]].monoid)

  implicit val iso = CartesianTests.Isomorphisms.invariant[Xor[String, ?]]

  checkAll("Xor[String, Int]", CartesianTests[Xor[String, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Xor, ?]", SerializableTests.serializable(Cartesian[Xor[String, ?]]))

  checkAll("Xor[String, NonEmptyList[Int]]", GroupLaws[Xor[String, NonEmptyList[Int]]].semigroup)

  implicit val eq0 = XorT.catsDataEqForXorT[Xor[String, ?], String, Int]

  checkAll("Xor[String, Int]", MonadErrorTests[Xor[String, ?], String].monadError[Int, Int, Int])
  checkAll("MonadError[Xor, String]", SerializableTests.serializable(MonadError[Xor[String, ?], String]))

  checkAll("Xor[String, Int]", MonadTests[Xor[String, ?]].monad[Int, Int, Int])
  checkAll("Monad[Xor[String, ?]]", SerializableTests.serializable(Monad[Xor[String, ?]]))

  checkAll("Xor[String, Int] with Option", TraverseTests[Xor[String, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Xor[String,?]]", SerializableTests.serializable(Traverse[Xor[String, ?]]))

  checkAll("Xor[Int, String]", OrderLaws[String Xor Int].order)

  checkAll("Xor[ListWrapper[String], ?]", SemigroupKTests[Xor[ListWrapper[String], ?]].semigroupK[Int])
  checkAll("SemigroupK[Xor[ListWrapper[String], ?]]", SerializableTests.serializable(SemigroupK[Xor[ListWrapper[String], ?]]))

  {
    implicit val S = ListWrapper.partialOrder[String]
    implicit val I = ListWrapper.partialOrder[Int]
    checkAll("ListWrapper[String] Xor ListWrapper[Int]", OrderLaws[ListWrapper[String] Xor ListWrapper[Int]].partialOrder)
    checkAll("PartialOrder[ListWrapper[String] Xor ListWrapper[Int]]", SerializableTests.serializable(PartialOrder[ListWrapper[String] Xor ListWrapper[Int]]))
  }

  {
    implicit val S = ListWrapper.eqv[String]
    implicit val I = ListWrapper.eqv[Int]
    checkAll("ListWrapper[String] Xor ListWrapper[Int]", OrderLaws[ListWrapper[String] Xor ListWrapper[Int]].eqv)
    checkAll("Eq[ListWrapper[String] Xor ListWrapper[Int]]", SerializableTests.serializable(Eq[ListWrapper[String] Xor ListWrapper[Int]]))
  }

  implicit val arbitraryXor: Arbitrary[Xor[Int, String]] = Arbitrary {
    for {
      left <- arbitrary[Boolean]
      xor <- if (left) arbitrary[Int].map(Xor.left)
             else arbitrary[String].map(Xor.right)
    } yield xor
  }

  checkAll("? Xor ?", BitraverseTests[Xor].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Xor]", SerializableTests.serializable(Bitraverse[Xor]))

  test("catchOnly catches matching exceptions") {
    assert(Xor.catchOnly[NumberFormatException]{ "foo".toInt }.isInstanceOf[Xor.Left[NumberFormatException]])
  }

  test("catchOnly lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Xor.catchOnly[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  test("catchNonFatal catches non-fatal exceptions") {
    assert(Xor.catchNonFatal{ "foo".toInt }.isLeft)
    assert(Xor.catchNonFatal{ throw new Throwable("blargh") }.isLeft)
  }

  test("fromTry is left for failed Try") {
    forAll { t: Try[Int] =>
      t.isFailure should === (Xor.fromTry(t).isLeft)
    }
  }

  test("fromEither isRight consistent with Either.isRight"){
    forAll { e: Either[Int, String] =>
      Xor.fromEither(e).isRight should === (e.isRight)
    }
  }

  test("fromOption isLeft consistent with Option.isEmpty") {
    forAll { (o: Option[Int], s: String) =>
      Xor.fromOption(o, s).isLeft should === (o.isEmpty)
    }
  }

  test("double swap is identity") {
    forAll { (x: Int Xor String) =>
      x.swap.swap should === (x)
    }
  }

  test("swap negates isLeft/isRight") {
    forAll { (x: Int Xor String) =>
      x.isLeft should !== (x.swap.isLeft)
      x.isRight should !== (x.swap.isRight)
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: Int Xor String) =>
      x.isLeft should !== (x.isRight)
    }
  }

  test("foreach is noop for left") {
    forAll { (x: Int Xor String) =>
      var count = 0
      x.foreach{ _ => count += 1}
      (count == 0) should === (x.isLeft)
    }
  }

  test("getOrElse ignores default for right") {
    forAll { (x: Int Xor String, s: String, t: String) =>
      if (x.isRight) {
        x.getOrElse(s) should === (x.getOrElse(t))
      }
    }
  }

  test("orElse") {
    forAll { (x: Int Xor String, y: Int Xor String) =>
      val z = x.orElse(y)
      (z === (x)) || (z === (y)) should === (true)
    }
  }

  test("recover recovers handled values") {
    val xor = Xor.left[String, Int]("xor")
    xor.recover { case "xor" => 5 }.isRight should === (true)
  }

  test("recover ignores unhandled values") {
    val xor = Xor.left[String, Int]("xor")
    xor.recover { case "notxor" => 5 } should === (xor)
  }

  test("recover ignores the right side") {
    val xor = Xor.right[String, Int](10)
    xor.recover { case "xor" => 5 } should === (xor)
  }

  test("recoverWith recovers handled values") {
    val xor = Xor.left[String, Int]("xor")
    xor.recoverWith { case "xor" => Xor.right[String, Int](5) }.isRight should === (true)
  }

  test("recoverWith ignores unhandled values") {
    val xor = Xor.left[String, Int]("xor")
    xor.recoverWith { case "notxor" => Xor.right[String, Int](5) } should === (xor)
  }

  test("recoverWith ignores the right side") {
    val xor = Xor.right[String, Int](10)
    xor.recoverWith { case "xor" => Xor.right[String, Int](5) } should === (xor)
  }

  test("valueOr consistent with swap then map then merge") {
    forAll { (x: Int Xor String, f: Int => String) =>
      x.valueOr(f) should === (x.swap.map(f).merge)
    }
  }

  test("isLeft implies forall") {
    forAll { (x: Int Xor String, p: String => Boolean) =>
      if (x.isLeft) {
        x.forall(p) should === (true)
      }
    }
  }

  test("isLeft implies exists is false") {
    forAll { (x: Int Xor String, p: String => Boolean) =>
      if (x.isLeft) {
        x.exists(p) should === (false)
      }
    }
  }

  test("ensure on left is identity") {
    forAll { (x: Int Xor String, i: Int, p: String => Boolean) =>
      if (x.isLeft) {
        x.ensure(i)(p) should === (x)
      }
    }
  }

  test("toIor then toXor is identity") {
    forAll { (x: Int Xor String) =>
      x.toIor.toXor should === (x)
    }
  }

  test("toTry then fromTry is identity") {
    implicit def eqTh: Eq[Throwable] = Eq.allEqual

    forAll { (x: Throwable Xor String) =>
      Xor.fromTry(x.toTry) should === (x)
    }
  }

  test("isLeft consistency") {
    forAll { (x: Int Xor String) =>
      x.isLeft should === (x.toEither.isLeft)
      x.isLeft should === (x.toOption.isEmpty)
      x.isLeft should === (x.toList.isEmpty)
      x.isLeft should === (x.toValidated.isInvalid)
      x.isLeft should === (x.toValidatedNel.isInvalid)
      Option(x.isLeft) should === (x.toXorT[Option].isLeft)
    }
  }

  test("withValidated") {
    forAll { (x: Int Xor String, f: Int => Double) =>
      x.withValidated(_.bimap(f, identity)) should === (x.leftMap(f))
    }
  }

  test("combine is right iff both operands are right") {
    forAll { (x: Int Xor String, y: Int Xor String) =>
      x.combine(y).isRight should === (x.isRight && y.isRight)
    }
  }

  test("to consistent with toList") {
    forAll { (x: Int Xor String) =>
      x.to[List, String] should === (x.toList)
    }
  }

  test("to consistent with toOption") {
    forAll { (x: Int Xor String) =>
      x.to[Option, String] should === (x.toOption)
    }
  }

  test("map2Eval is lazy") {
    val bomb: Eval[String Xor Int] = Later(sys.error("boom"))
    val x = Xor.left[String, Int]("l")
    x.map2Eval(bomb)(_ + _).value should === (x)
  }

}
