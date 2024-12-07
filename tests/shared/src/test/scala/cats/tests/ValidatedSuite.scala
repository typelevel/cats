/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.{
  Align,
  Applicative,
  ApplicativeError,
  Bitraverse,
  CommutativeApplicative,
  SemigroupK,
  Semigroupal,
  Show,
  Traverse
}
import cats.data.{EitherT, Ior, NonEmptyChain, NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.{Eq, Order, PartialOrder, Semigroup}
import cats.kernel.laws.discipline.{EqTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupKTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.validated.*
import org.scalacheck.Arbitrary.*
import scala.util.Try
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class ValidatedSuite extends CatsSuite {
  implicit val iso: Isomorphisms[Validated[String, *]] = Isomorphisms.invariant[Validated[String, *]]
  checkAll("Validated[String, Int]", SemigroupalTests[Validated[String, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Validated[String,*]]", SerializableTests.serializable(Semigroupal[Validated[String, *]]))

  checkAll("Validated[*, *]", BitraverseTests[Validated].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Validated]", SerializableTests.serializable(Bitraverse[Validated]))

  implicit val eq0: Eq[EitherT[Validated[String, *], String, Int]] =
    EitherT.catsDataEqForEitherT[Validated[String, *], String, Int]

  checkAll("Validated[String, Int]",
           ApplicativeErrorTests[Validated[String, *], String].applicativeError[Int, Int, Int]
  )
  checkAll("ApplicativeError[Validated, String]",
           SerializableTests.serializable(ApplicativeError[Validated[String, *], String])
  )

  checkAll("Validated[String, Int] with Option",
           TraverseTests[Validated[String, *]].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[Validated[String, *]]", SerializableTests.serializable(Traverse[Validated[String, *]]))

  checkAll("Validated[String, Int]", OrderTests[Validated[String, Int]].order)
  checkAll("Order[Validated[String, Int]]", SerializableTests.serializable(Order[Validated[String, Int]]))

  checkAll("Validated[String, Int]", MonoidTests[Validated[String, Int]].monoid)

  checkAll("Validated[String, NonEmptyList[Int]]", SemigroupTests[Validated[String, NonEmptyList[Int]]].semigroup)

  checkAll("Validated[Int, Int]", CommutativeApplicativeTests[Validated[Int, *]].commutativeApplicative[Int, Int, Int])
  checkAll("CommutativeApplicative[Validated[Int, *]]",
           SerializableTests.serializable(CommutativeApplicative[Validated[Int, *]])
  )

  checkAll("Validated[Int, Int]", AlignTests[Validated[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[Validated[Int, *]]", SerializableTests.serializable(Align[Validated[Int, *]]))

  {
    implicit val L: Semigroup[ListWrapper[String]] = ListWrapper.semigroup[String]
    checkAll("Validated[ListWrapper[String], *]", SemigroupKTests[Validated[ListWrapper[String], *]].semigroupK[Int])
    checkAll("SemigroupK[Validated[ListWrapper[String], *]]",
             SerializableTests.serializable(SemigroupK[Validated[ListWrapper[String], *]])
    )
  }

  {
    implicit val S: PartialOrder[ListWrapper[String]] = ListWrapper.partialOrder[String]
    implicit val I: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("Validated[ListWrapper[String], ListWrapper[Int]]",
             PartialOrderTests[Validated[ListWrapper[String], ListWrapper[Int]]].partialOrder
    )
    checkAll(
      "PartialOrder[Validated[ListWrapper[String], ListWrapper[Int]]]",
      SerializableTests.serializable(PartialOrder[Validated[ListWrapper[String], ListWrapper[Int]]])
    )
  }

  {
    implicit val S: Eq[ListWrapper[String]] = ListWrapper.eqv[String]
    implicit val I: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("Validated[ListWrapper[String], ListWrapper[Int]]",
             EqTests[Validated[ListWrapper[String], ListWrapper[Int]]].eqv
    )
    checkAll("Eq[Validated[ListWrapper[String], ListWrapper[Int]]]",
             SerializableTests.serializable(Eq[Validated[ListWrapper[String], ListWrapper[Int]]])
    )
  }

  test("ap2 combines failures in order") {
    val plus = (_: Int) + (_: Int)
    assert(Applicative[Validated[String, *]].ap2(Valid(plus))(Invalid("1"), Invalid("2")) === (Invalid("12")))
  }

  test("catchOnly catches matching exceptions") {
    assert(Validated.catchOnly[NumberFormatException]("foo".toInt).isInstanceOf[Invalid[NumberFormatException]])
  }

  test("catchOnly lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Validated.catchOnly[IndexOutOfBoundsException]("foo".toInt)
    }
  }

  test("catchNonFatal catches non-fatal exceptions") {
    assert(Validated.catchNonFatal("foo".toInt).isInvalid)
    assert(Validated.catchNonFatal(throw new Throwable("blargh")).isInvalid)
  }

  test("fromTry is invalid for failed try") {
    forAll { (t: Try[Int]) =>
      assert(t.isFailure === (Validated.fromTry(t).isInvalid))
    }
  }

  test("ValidatedNel") {
    forAll { (e: String) =>
      val manual = Validated.invalid[NonEmptyList[String], Int](NonEmptyList.of(e))
      assert(Validated.invalidNel[String, Int](e) === manual)
      assert(Validated.invalid[String, Int](e).toValidatedNel === manual)
    }
  }

  test("ValidatedNec") {
    forAll { (e: String) =>
      val manual = Validated.invalid[NonEmptyChain[String], Int](NonEmptyChain.one(e))
      assert(Validated.invalidNec[String, Int](e) === manual)
      assert(Validated.invalid[String, Int](e).toValidatedNec === manual)
    }
  }

  test("isInvalid consistent with forall and exists") {
    forAll { (v: Validated[String, Int], p: Int => Boolean) =>
      if (v.isInvalid) {
        assert(v.forall(p) === true)
        assert(v.exists(p) === false)
      } else {
        assert(v.forall(p) === (v.exists(p)))
      }
    }
  }

  test("foreach only runs for valid") {
    forAll { (v: Validated[String, Int]) =>
      var count = 0
      v.foreach(_ => count += 1)
      assert(v.isValid === (count == 1))
      assert(v.isInvalid === (count == 0))
    }
  }

  test("getOrElse consistent with orElse") {
    forAll { (v: Validated[String, Int], u: Validated[String, Int], i: Int) =>
      assert(v.getOrElse(u.getOrElse(i)) === (v.orElse(u).getOrElse(i)))
    }
  }

  test("findValid accumulates failures") {
    forAll { (v: Validated[String, Int], u: Validated[String, Int]) =>
      assertEquals(v.findValid(u), {
                     (v, u) match {
                       case (vv @ Valid(_), _)         => vv
                       case (_, uu @ Valid(_))         => uu
                       case (Invalid(s1), Invalid(s2)) => Invalid(s1 ++ s2)
                     }
                   }
      )
    }
  }

  test("orElse ignores left failure") {
    forAll { (v: Validated[String, Int], u: Validated[String, Int]) =>
      assertEquals(v.orElse(u), {
                     (v, u) match {
                       case (vv @ Valid(_), _) => vv
                       case (_, uu)            => uu
                     }
                   }
      )
    }
  }

  test("valueOr consistent with swap then map then merge") {
    forAll { (v: Validated[String, Int], f: String => Int) =>
      assert(v.valueOr(f) === (v.swap.map(f).merge))
    }
  }

  test("toEither then fromEither is identity") {
    forAll { (v: Validated[String, Int]) =>
      assert(Validated.fromEither(v.toEither) === v)
    }
  }

  test("toList and toOption are empty for invalid") {
    forAll { (v: Validated[String, Int]) =>
      assert(v.isInvalid === (v.toList.isEmpty))
      assert(v.isInvalid === (v.toOption.isEmpty))
    }
  }

  test("show isn't empty") {
    forAll { (v: Validated[String, Int]) =>
      val show = implicitly[Show[Validated[String, Int]]]
      assert(show.show(v).nonEmpty === true)
    }
  }

  test("andThen consistent with Either's flatMap") {
    forAll { (v: Validated[String, Int], f: Int => Validated[String, Int]) =>
      assert(v.andThen(f) === (v.withEither(_.flatMap(f(_).toEither))))
    }
  }

  test("ad-hoc andThen tests") {
    def even(i: Int): Validated[String, Int] =
      if (i % 2 == 0) Validated.valid(i)
      else Validated.invalid(s"$i is not even")

    assert((Validated.valid(3).andThen(even)) === (Validated.invalid("3 is not even")))
    assert((Validated.valid(4).andThen(even)) === (Validated.valid(4)))
    assert((Validated.invalid("foo").andThen(even)) === (Validated.invalid("foo")))
  }

  test("fromOption consistent with Either.fromOption") {
    forAll { (o: Option[Int], s: String) =>
      assert(Validated.fromOption(o, s) === (Either.fromOption(o, s).toValidated))
    }
  }

  test("fromOption consistent with toOption") {
    forAll { (o: Option[Int], s: String) =>
      assert(Validated.fromOption(o, s).toOption === o)
    }
  }

  test("fromIor consistent with Ior.toValidated") {
    forAll { (i: Ior[String, Int]) =>
      assert(Validated.fromIor(i) === (i.toValidated))
    }
  }

  test("toIor then fromEither is identity") {
    forAll { (v: Validated[String, Int]) =>
      assert(Validated.fromIor(v.toIor) === v)
    }
  }

  test("isValid after combine, iff both are valid") {
    forAll { (lhs: Validated[Int, String], rhs: Validated[Int, String]) =>
      assert(lhs.combine(rhs).isValid === (lhs.isValid && rhs.isValid))
    }
  }

  test("isInvalid consistent with isValid") {
    forAll { (x: Validated[String, Int]) =>
      assert(x.isInvalid =!= x.isValid)
    }
  }

  test("double swap is identity") {
    forAll { (x: Validated[String, Int]) =>
      assert(x.swap.swap === x)
    }
  }

  test("swap negates isInvalid/isValid") {
    forAll { (x: Validated[String, Int]) =>
      assert(x.isInvalid =!= x.swap.isInvalid)
      assert(x.isValid =!= x.swap.isValid)
    }
  }

  test("Unapply-based apply syntax") {
    // this type has kind F[_, _], which requires `Unapply`-based syntax
    val x: ValidatedNel[String, Int] = Validated.invalidNel("error 1")
    val y: ValidatedNel[String, Boolean] = Validated.invalidNel("error 2")

    val z = x.map2(y)((i, b) => if (b) i + 1 else i)
    assert(z === (NonEmptyList.of("error 1", "error 2").invalid[Int]))
  }

  test("ensure on Invalid is identity") {
    forAll { (x: Validated[Int, String], i: Int, p: String => Boolean) =>
      if (x.isInvalid) {
        assert(x.ensure(i)(p) === x)
      }
    }
  }

  test("ensure should fail if predicate not satisfied") {
    forAll { (x: Validated[String, Int], s: String, p: Int => Boolean) =>
      if (x.exists(!p(_))) {
        assert(x.ensure(s)(p) === (Validated.invalid(s)))
      }
    }
  }

  test("ensureOr on Invalid is identity") {
    forAll { (x: Validated[Int, String], f: String => Int, p: String => Boolean) =>
      if (x.isInvalid) {
        assert(x.ensureOr(f)(p) === x)
      }
    }
  }

  test("ensureOr should fail if predicate not satisfied") {
    forAll { (x: Validated[String, Int], f: Int => String, p: Int => Boolean) =>
      if (x.exists(!p(_))) {
        assert(x.ensureOr(f)(p).isInvalid)
      }
    }
  }

  test("cond consistent with Either.cond + toValidated") {
    forAll { (cond: Boolean, s: String, i: Int) =>
      assert(Validated.cond(cond, s, i) === (Either.cond(cond, s, i).toValidated))
    }
  }

  test("condNel consistent with Either.cond + toValidatedNel") {
    forAll { (cond: Boolean, s: String, i: Int) =>
      assert(Validated.condNel(cond, s, i) === (Either.cond(cond, s, i).toValidatedNel))
    }
  }

  test("condNec consistent with Either.cond + toValidatedNec") {
    forAll { (cond: Boolean, s: String, i: Int) =>
      assert(Validated.condNec(cond, s, i) === (Either.cond(cond, s, i).toValidatedNec))
    }
  }

  test("liftTo consistent with direct to Option") {
    forAll { (v: Validated[Unit, Int]) =>
      assertEquals(v.liftTo[Option], v.toOption)
    }
  }

  test("liftTo works with specialized errors") {
    implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals
    val ex: IllegalArgumentException = new IllegalArgumentException()
    val validated: Validated[IllegalArgumentException, Int] = Validated.Invalid(ex)
    val lifted: Either[Throwable, Int] = validated.liftTo[Either[Throwable, *]]

    assert(lifted === (Left[Throwable, Int](ex)))
  }
}
