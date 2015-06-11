package cats
package tests

import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.laws.discipline.{TraverseTests, ApplicativeTests, SerializableTests}
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Prop.BooleanOperators
import cats.laws.discipline.arbitrary._

import scala.util.{Failure, Success, Try}

class ValidatedTests extends CatsSuite {
  checkAll("Validated[String, Int]", ApplicativeTests[Validated[String,?]].applicative[Int, Int, Int])
  checkAll("Applicative[Validated[String,?]]", SerializableTests.serializable(Applicative[Validated[String,?]]))

  checkAll("Validated[String, Int] with Option", TraverseTests[Validated[String,?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Validated[String,?]]", SerializableTests.serializable(Traverse[Validated[String,?]]))

  implicit val arbitraryTryInt: Arbitrary[Try[Int]] = Arbitrary {
    for {
      success <- arbitrary[Boolean]
      t <- if (success) arbitrary[Int].map(Success(_))
           else arbitrary[Throwable].map(Failure(_))
    } yield t
  }

  test("ap2 combines failures in order") {
    val plus = (_: Int) + (_: Int)
    assert(Applicative[Validated[String, ?]].ap2(Invalid("1"), Invalid("2"))(Valid(plus)) == Invalid("12"))
  }

  test("fromTryCatch catches matching exceptions") {
    assert(Validated.fromTryCatch[NumberFormatException]{ "foo".toInt }.isInstanceOf[Invalid[NumberFormatException]])
  }

  test("fromTryCatch lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Validated.fromTryCatch[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  check{
    forAll { t: Try[Int] =>
      t.isFailure == Validated.fromTry(t).isInvalid
    }
  }

  test("filter makes non-matching entries invalid") {
    assert(
      (for {
        x <- Valid(1).filter[String](_ % 2 == 0)
      } yield ()).isInvalid)
  }

  test("filter leaves matching entries valid") {
    assert(
      (for {
        _ <- Valid(2).filter[String](_ % 2 == 0)
      } yield ()).isValid)
  }

  check {
    forAll { (v: Validated[String, Int], p: Int => Boolean) =>
      v.isInvalid ==> (v.forall(p) && !v.exists(p))
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      var count = 0
      v.foreach(_ => count += 1)
      v.isValid == (count == 1)
    }
  }

  check {
    forAll { (v: Validated[String, Int], u: Validated[String, Int], i: Int) =>
      v.getOrElse(u.getOrElse(i)) == v.orElse(u).getOrElse(i)
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      Validated.fromEither(v.toEither) == v
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      v.isInvalid == v.toList.isEmpty &&
      v.isInvalid == v.toOption.isEmpty
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      val equality = implicitly[Eq[Validated[String, Int]]]
      equality.eqv(v, v)
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      val partialOrder = implicitly[PartialOrder[Validated[String, Int]]]
      partialOrder.partialCompare(v, v) == 0 &&
          partialOrder.eqv(v, v)
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      val order = implicitly[Order[Validated[String, Int]]]
      order.compare(v, v) == 0 &&
          order.partialCompare(v, v) == 0 &&
          order.eqv(v, v)
    }
  }

  check {
    forAll { (v: Validated[String, Int]) =>
      val show = implicitly[Show[Validated[String, Int]]]
      show.show(v).size > 0
    }
  }
}
