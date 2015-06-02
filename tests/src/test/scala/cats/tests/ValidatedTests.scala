package cats
package tests

import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.std.string._
import cats.laws.discipline.{TraverseTests, ApplicativeTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class ValidatedTests extends CatsSuite {
  checkAll("Validated[String, Int]", ApplicativeTests[Validated[String,?]].applicative[Int, Int, Int])
  checkAll("Applicative[Validated[String,?]]", SerializableTests.serializable(Applicative[Validated[String,?]]))

  checkAll("Validated[String, Int] with Option", TraverseTests[Validated[String,?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Validated[String,?]]", SerializableTests.serializable(Traverse[Validated[String,?]]))

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
}
