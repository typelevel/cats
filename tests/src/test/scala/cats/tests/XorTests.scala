package cats
package tests

import cats.data.Xor
import cats.data.Xor._
import cats.laws.discipline.arbitrary.xorArbitrary
import cats.laws.discipline.{BifunctorTests, TraverseTests, MonadErrorTests, SerializableTests}
import algebra.laws.{GroupLaws, OrderLaws}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

import scala.util.Try

class XorTests extends CatsSuite {
  checkAll("Xor[String, Int]", GroupLaws[Xor[String, Int]].monoid)

  checkAll("Xor[String, Int]", MonadErrorTests[Xor[String, ?], String].monadError[Int, Int, Int])
  checkAll("MonadError[Xor, String]", SerializableTests.serializable(MonadError[Xor[String, ?], String]))

  checkAll("Xor[String, Int] with Option", TraverseTests[Xor[String, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Xor[String,?]]", SerializableTests.serializable(Traverse[Xor[String, ?]]))

  checkAll("Xor[Int, String]", OrderLaws[String Xor Int].order)

  implicit val arbitraryXor: Arbitrary[Xor[Int, String]] = Arbitrary {
    for {
      left <- arbitrary[Boolean]
      xor <- if (left) arbitrary[Int].map(Xor.left)
             else arbitrary[String].map(Xor.right)
    } yield xor
  }

  checkAll("? Xor ?", BifunctorTests[Xor].bifunctor[Int, Int, Int, String, String, String])

  test("fromTryCatch catches matching exceptions") {
    assert(Xor.fromTryCatch[NumberFormatException]{ "foo".toInt }.isInstanceOf[Xor.Left[NumberFormatException]])
  }

  test("fromTryCatch lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Xor.fromTryCatch[IndexOutOfBoundsException]{ "foo".toInt }
    }
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

  test("foreach is noop for left") {
    forAll { (x: Int Xor String) =>
      var count = 0
      x.foreach{ _ => count += 1}
      (count == 0) should === (x.isLeft)
    }
  }

  test("getOrElse ignores default for right") {
    forAll { (x: Int Xor String, s: String, t: String) =>
      whenever(x.isRight) {
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
      whenever(x.isLeft) {
        x.forall(p) should === (true)
      }
    }
  }

  test("isLeft implies exists is false") {
    forAll { (x: Int Xor String, p: String => Boolean) =>
      whenever(x.isLeft) {
        x.exists(p) should === (false)
      }
    }
  }

  test("ensure on left is identity") {
    forAll { (x: Int Xor String, i: Int, p: String => Boolean) =>
      whenever(x.isLeft) {
        x.ensure(i)(p) should === (x)
      }
    }
  }

  test("toIor then toXor is identity") {
    forAll { (x: Int Xor String) =>
      x.toIor.toXor should === (x)
    }
  }

  test("isLeft consistency") {
    forAll { (x: Int Xor String) =>
      x.isLeft should === (x.toEither.isLeft)
      x.isLeft should === (x.toOption.isEmpty)
      x.isLeft should === (x.toList.isEmpty)
      x.isLeft should === (x.toValidated.isInvalid)
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

}
