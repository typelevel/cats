package cats
package tests

import cats.data.{NonEmptyList, Validated, ValidatedNel, XorT}
import cats.data.Validated.{Valid, Invalid}
import cats.laws.discipline.{BitraverseTests, TraverseTests, ApplicativeErrorTests, SerializableTests, CartesianTests}
import org.scalacheck.Arbitrary._
import cats.laws.discipline.{SemigroupKTests}
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.{OrderLaws, GroupLaws}

import scala.util.Try

class ValidatedTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Validated[String, ?]]
  checkAll("Validated[String, Int]", CartesianTests[Validated[String,?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Validated[String,?]]", SerializableTests.serializable(Cartesian[Validated[String,?]]))

  checkAll("Validated[?, ?]", BitraverseTests[Validated].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Validated]", SerializableTests.serializable(Bitraverse[Validated]))

  implicit val eq0 = XorT.catsDataEqForXorT[Validated[String, ?], String, Int]

  checkAll("Validated[String, Int]", ApplicativeErrorTests[Validated[String, ?], String].applicativeError[Int, Int, Int])
  checkAll("ApplicativeError[Validated, String]", SerializableTests.serializable(ApplicativeError[Validated[String, ?], String]))

  checkAll("Validated[String, Int] with Option", TraverseTests[Validated[String,?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Validated[String, ?]]", SerializableTests.serializable(Traverse[Validated[String,?]]))

  checkAll("Validated[String, Int]", OrderLaws[Validated[String, Int]].order)
  checkAll("Order[Validated[String, Int]]", SerializableTests.serializable(Order[Validated[String, Int]]))

  checkAll("Validated[String, Int]", GroupLaws[Validated[String, Int]].monoid)

  checkAll("Validated[String, NonEmptyList[Int]]", GroupLaws[Validated[String, NonEmptyList[Int]]].semigroup)

  {
    implicit val L = ListWrapper.semigroup[String]
    checkAll("Validated[ListWrapper[String], ?]", SemigroupKTests[Validated[ListWrapper[String], ?]].semigroupK[Int])
    checkAll("SemigroupK[Validated[ListWrapper[String], ?]]", SerializableTests.serializable(SemigroupK[Validated[ListWrapper[String], ?]]))
  }

  {
    implicit val S = ListWrapper.partialOrder[String]
    implicit val I = ListWrapper.partialOrder[Int]
    checkAll("Validated[ListWrapper[String], ListWrapper[Int]]", OrderLaws[Validated[ListWrapper[String], ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[Validated[ListWrapper[String], ListWrapper[Int]]]", SerializableTests.serializable(PartialOrder[Validated[ListWrapper[String], ListWrapper[Int]]]))
  }

  {
    implicit val S = ListWrapper.eqv[String]
    implicit val I = ListWrapper.eqv[Int]
    checkAll("Validated[ListWrapper[String], ListWrapper[Int]]", OrderLaws[Validated[ListWrapper[String], ListWrapper[Int]]].eqv)
    checkAll("Eq[Validated[ListWrapper[String], ListWrapper[Int]]]", SerializableTests.serializable(Eq[Validated[ListWrapper[String], ListWrapper[Int]]]))
  }

  test("ap2 combines failures in order") {
    val plus = (_: Int) + (_: Int)
    Applicative[Validated[String, ?]].ap2(Valid(plus))(Invalid("1"), Invalid("2")) should === (Invalid("12"))
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
      val manual = Validated.invalid[NonEmptyList[String], Int](NonEmptyList.of(e))
      Validated.invalidNel[String, Int](e) should === (manual)
      Validated.invalid[String, Int](e).toValidatedNel should === (manual)
    }
  }

  test("isInvalid consistent with forall and exists") {
    forAll { (v: Validated[String, Int], p: Int => Boolean) =>
      if (v.isInvalid) {
        v.forall(p) should === (true)
        v.exists(p) should === (false)
      } else {
        v.forall(p) should === (v.exists(p))
      }
    }
  }

  test("foreach only runs for valid") {
    forAll { (v: Validated[String, Int]) =>
      var count = 0
      v.foreach(_ => count += 1)
      v.isValid should === (count == 1)
      v.isInvalid should === (count == 0)
    }
  }

  test("getOrElse consistent with orElse") {
    forAll { (v: Validated[String, Int], u: Validated[String, Int], i: Int) =>
      v.getOrElse(u.getOrElse(i)) should === (v.orElse(u).getOrElse(i))
    }
  }

  test("valueOr consistent with swap then map then merge") {
    forAll { (v: Validated[String, Int], f: String => Int) =>
      v.valueOr(f) should === (v.swap.map(f).merge)
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

  test("fromOption consistent with Either.fromOption"){
    forAll { (o: Option[Int], s: String) =>
      Validated.fromOption(o, s) should === (Either.fromOption(o, s).toValidated)
    }
  }

  test("fromOption consistent with toOption"){
    forAll { (o: Option[Int], s: String) =>
      Validated.fromOption(o, s).toOption should === (o)
    }
  }

  test("isValid after combine, iff both are valid") {
    forAll { (lhs: Validated[Int, String], rhs: Validated[Int, String]) =>
      lhs.combine(rhs).isValid should === (lhs.isValid && rhs.isValid)
    }
  }

  test("isInvalid consistent with isValid") {
    forAll { (x: Validated[String, Int]) =>
      x.isInvalid should !== (x.isValid)
    }
  }

  test("double swap is identity") {
    forAll { (x: Validated[String, Int]) =>
      x.swap.swap should ===(x)
    }
  }

  test("swap negates isInvalid/isValid") {
    forAll { (x: Validated[String, Int]) =>
      x.isInvalid should !== (x.swap.isInvalid)
      x.isValid should !== (x.swap.isValid)
    }
  }

  test("Unapply-based apply syntax"){
    // this type has kind F[_, _], which requires `Unapply`-based syntax
    val x: ValidatedNel[String, Int] = Validated.invalidNel("error 1")
    val y: ValidatedNel[String, Boolean] = Validated.invalidNel("error 2")

    val z = x.map2(y)((i, b) => if (b) i + 1 else i)
    z should === (NonEmptyList.of("error 1", "error 2").invalid[Int])
  }

  test("ensure on Invalid is identity") {
    forAll { (x: Validated[Int,String], i: Int, p: String => Boolean) =>
      if (x.isInvalid) {
        x.ensure(i)(p) should === (x)
      }
    }
  }

  test("ensure should fail if predicate not satisfied") {
    forAll { (x: Validated[String, Int], s: String, p: Int => Boolean) =>
      if (x.exists(!p(_))) {
        x.ensure(s)(p) should === (Validated.invalid(s))
      }
    }
  }
}
