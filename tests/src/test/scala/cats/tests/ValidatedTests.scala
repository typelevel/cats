package cats
package tests

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Valid, Invalid}
import cats.laws.discipline.{BifunctorTests, TraverseTests, ApplicativeTests, SerializableTests}
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary._
import cats.laws.discipline.arbitrary._
import algebra.laws.OrderLaws

import scala.util.Try

class ValidatedTests extends CatsSuite {
  checkAll("Validated[String, Int]", ApplicativeTests[Validated[String,?]].applicative[Int, Int, Int])
  checkAll("Validated[?, ?]", BifunctorTests[Validated].bifunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Applicative[Validated[String,?]]", SerializableTests.serializable(Applicative[Validated[String,?]]))

  checkAll("Validated[String, Int] with Option", TraverseTests[Validated[String,?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Validated[String,?]]", SerializableTests.serializable(Traverse[Validated[String,?]]))

  checkAll("Validated[String, Int]", OrderLaws[Validated[String, Int]].order)

  test("ap2 combines failures in order") {
    val plus = (_: Int) + (_: Int)
    Applicative[Validated[String, ?]].ap2(Invalid("1"), Invalid("2"))(Valid(plus)) should === (Invalid("12"))
  }

  test("fromTryCatch catches matching exceptions") {
    assert(Validated.fromTryCatch[NumberFormatException]{ "foo".toInt }.isInstanceOf[Invalid[NumberFormatException]])
  }

  test("fromTryCatch lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Validated.fromTryCatch[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  test("fromTry is invalid for failed try"){
    forAll { t: Try[Int] =>
      t.isFailure should === (Validated.fromTry(t).isInvalid)
    }
  }

  test("filter makes non-matching entries invalid") {
    Valid(1).filter[String](_ % 2 == 0).isInvalid should ===(true)
  }

  test("filter leaves matching entries valid") {
    Valid(2).filter[String](_ % 2 == 0).isValid should ===(true)
  }

  test("ValidatedNel") {
    forAll { (e: String) =>
      val manual = Validated.invalid[NonEmptyList[String], Int](NonEmptyList(e))
      Validated.invalidNel[String, Int](e) should === (manual)
      Validated.invalid[String, Int](e).toValidatedNel should === (manual)
    }
  }

  test("isInvalid consistent with forall and exists") {
    forAll { (v: Validated[String, Int], p: Int => Boolean) =>
      whenever(v.isInvalid) {
        v.forall(p) should === (true)
        v.exists(p) should === (false)
      }
    }
  }

  test("foreach only runs for valid") {
    forAll { (v: Validated[String, Int]) =>
      var count = 0
      v.foreach(_ => count += 1)
      v.isValid should === (count == 1)
    }
  }

  test("getOrElse consistent with orElse") {
    forAll { (v: Validated[String, Int], u: Validated[String, Int], i: Int) =>
      v.getOrElse(u.getOrElse(i)) should === (v.orElse(u).getOrElse(i))
    }
  }

  test("toEither then fromEither is identity") {
    forAll { (v: Validated[String, Int]) =>
      Validated.fromEither(v.toEither) should === (v)
    }
  }

  test("toList and toOption are empty for invalid") {
    forAll { (v: Validated[String, Int]) =>
      v.isInvalid should === (v.toList.isEmpty)
      v.isInvalid should === (v.toOption.isEmpty)
    }
  }

  test("show isn't empty") {
    forAll { (v: Validated[String, Int]) =>
      val show = implicitly[Show[Validated[String, Int]]]
      show.show(v).nonEmpty should === (true)
    }
  }
}
