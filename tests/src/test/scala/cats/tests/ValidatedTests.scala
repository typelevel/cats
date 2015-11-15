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

  test("catchOnly catches matching exceptions") {
    assert(Validated.catchOnly[NumberFormatException]{ "foo".toInt }.isInstanceOf[Invalid[NumberFormatException]])
  }

  test("catchOnly lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Validated.catchOnly[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  test("catchNonFatal catches non-fatal exceptions") {
    assert(Validated.catchNonFatal{ "foo".toInt }.isInvalid)
    assert(Validated.catchNonFatal{ throw new Throwable("blargh") }.isInvalid)
  }

  test("fromTry is invalid for failed try"){
    forAll { t: Try[Int] =>
      t.isFailure should === (Validated.fromTry(t).isInvalid)
    }
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
      if (v.isInvalid) {
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

  test("andThen consistent with Xor's flatMap"){
    forAll { (v: Validated[String, Int], f: Int => Validated[String, Int]) =>
      v.andThen(f) should === (v.withXor(_.flatMap(f(_).toXor)))
    }
  }

  test("ad-hoc andThen tests"){
    def even(i: Int): Validated[String, Int] =
      if (i % 2 == 0) Validated.valid(i)
      else Validated.invalid(s"$i is not even")

    (Validated.valid(3) andThen even) should === (Validated.invalid("3 is not even"))
    (Validated.valid(4) andThen even) should === (Validated.valid(4))
    (Validated.invalid("foo") andThen even) should === (Validated.invalid("foo"))
  }
}
