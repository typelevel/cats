package cats
package tests

import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.std.string._
import cats.laws.discipline.{ApplicativeTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class ValidatedTests extends CatsSuite {
  checkAll("Validated[String, Int]", ApplicativeTests[Validated[String,?]].applicative[Int, Int, Int])
  checkAll("Applicative[Validated[String,?]]", SerializableTests.serializable(Applicative[Validated[String,?]]))

  test("ap2 combines failures in order") {
    val plus = (_: Int) + (_: Int)
    assert(Validated.validatedInstances[String].ap2(Invalid("1"), Invalid("2"))(Valid(plus)) == Invalid("12"))
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
    for {
      x <- Valid(1).filter[String](_ % 2 == 0)
    } fail("1 is not even")
  }

  test("filter leaves matching entries valid") {
    assert(
      (for {
        _ <- Valid(2).filter[String](_ % 2 == 0)
      } yield ()).isValid)
  }
}
