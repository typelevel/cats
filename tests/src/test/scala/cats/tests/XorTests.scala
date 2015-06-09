package cats
package tests

import cats.data.Xor
import cats.data.Xor._
import cats.laws.discipline.{TraverseTests, MonadTests, SerializableTests}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Arbitrary._

import scala.util.{Failure, Success, Try}

class XorTests extends CatsSuite {
  checkAll("Xor[String, Int]", MonadTests[String Xor ?].monad[Int, Int, Int])
  checkAll("Monad[String Xor ?]", SerializableTests.serializable(Monad[String Xor ?]))

  checkAll("Xor[String, Int] with Option", TraverseTests[Xor[String, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Xor[String,?]]", SerializableTests.serializable(Traverse[Xor[String, ?]]))

  implicit val arbitraryXor: Arbitrary[Xor[Int, String]] = Arbitrary {
    for {
      left <- arbitrary[Boolean]
      xor <- if (left) arbitrary[Int].map(Xor.left)
             else arbitrary[String].map(Xor.right)
    } yield xor
  }

  implicit val arbitraryTryInt: Arbitrary[Try[Int]] = Arbitrary {
    for {
      success <- arbitrary[Boolean]
      t <- if (success) arbitrary[Int].map(Success(_))
           else Gen.const(Failure(new Throwable {}))
    } yield t
  }

  test("fromTryCatch catches matching exceptions") {
    assert(Xor.fromTryCatch[NumberFormatException]{ "foo".toInt }.isInstanceOf[Xor.Left[NumberFormatException]])
  }

  test("fromTryCatch lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Xor.fromTryCatch[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  check{
    forAll { t: Try[Int] =>
      t.isFailure == Xor.fromTry(t).isLeft
    }
  }

  check{
    forAll { e: Either[Int, String] =>
      Xor.fromEither(e).isRight == e.isRight
    }
  }

  check{
    forAll { (o: Option[Int], s: String) =>
      Xor.fromOption(o, s).isLeft == o.isEmpty
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      x.swap.swap == x
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      var count = 0
      x.foreach{ _ => count += 1}
      (count == 0) == x.isLeft
    }
  }

  check {
    forAll { (x: Int Xor String, s: String, t: String) =>
      x.isRight ==> (x.getOrElse(s) == x.getOrElse(t))
    }
  }

  check {
    forAll { (x: Int Xor String, y: Int Xor String) =>
      (x.orElse(y) == x) || (x.orElse(y) == y)
    }
  }

  check {
    forAll { (x: Int Xor String, f: Int => String) =>
      x.valueOr(f) == x.swap.map(f).merge
    }
  }

  check {
    forAll { (x: Int Xor String, p: String => Boolean) =>
      x.isLeft ==> x.forall(p)
    }
  }

  check {
    forAll { (x: Int Xor String, p: String => Boolean) =>
      x.isLeft ==> !x.exists(p)
    }
  }

  check {
    forAll { (x: Int Xor String, i: Int, p: String => Boolean) =>
      x.isLeft ==> (x.ensure(i)(p) == x)
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      x.toIor.toXor == x
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      x.toEither.isLeft == x.isLeft &&
      x.toOption.isEmpty == x.isLeft &&
      x.toList.isEmpty == x.isLeft &&
      x.toValidated.isInvalid == x.isLeft
    }
  }

  check {
    forAll { (x: Int Xor String, f: Int => Double) =>
      x.withValidated(_.bimap(f, identity)) == x.leftMap(f)
    }
  }

  check {
    forAll { (x: Int Xor String, y: Int Xor String) =>
      x.combine(y).isRight == (x.isRight && y.isRight)
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      val equality = implicitly[Eq[Int Xor String]]
      equality.eqv(x, x)
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      val partialOrder = implicitly[PartialOrder[Int Xor String]]
      partialOrder.partialCompare(x, x) == 0 &&
      partialOrder.eqv(x, x)
    }
  }

  check {
    forAll { (x: Int Xor String) =>
      val order = implicitly[Order[Int Xor String]]
      order.compare(x, x) == 0 &&
      order.partialCompare(x, x) == 0 &&
      order.eqv(x, x)
    }
  }
}
