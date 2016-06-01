package cats
package tests

import cats.arrow.{Arrow, Split}
import cats.data.{Cokleisli, NonEmptyList}
import cats.functor.Profunctor
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary
import cats.laws.discipline.{SemigroupKTests, MonoidKTests}

class CokleisliTests extends SlowCatsSuite {

  implicit def cokleisliEq[F[_], A, B](implicit A: Arbitrary[F[A]], FB: Eq[B]): Eq[Cokleisli[F, A, B]] =
    Eq.by[Cokleisli[F, A, B], F[A] => B](_.run)

  def cokleisliEqE[F[_], A](implicit A: Arbitrary[F[A]], FA: Eq[A]): Eq[Cokleisli[F, A, A]] =
    Eq.by[Cokleisli[F, A, A], F[A] => A](_.run)

  implicit val iso = CartesianTests.Isomorphisms.invariant[Cokleisli[Option, Int, ?]]

  checkAll("Cokleisli[Option, Int, Int]", CartesianTests[Cokleisli[Option, Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Cokleisli[Option, Int, ?]", SerializableTests.serializable(Cartesian[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ApplicativeTests[Cokleisli[Option, Int, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Cokleisli[Option, Int, ?]", SerializableTests.serializable(Applicative[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ProfunctorTests[Cokleisli[Option, ?, ?]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Profunctor[Cokleisli[Option, ?, ?]", SerializableTests.serializable(Profunctor[Cokleisli[Option, ?, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", SplitTests[Cokleisli[Option, ?, ?]].split[Int, Int, Int, Int, Int, Int])
  checkAll("Split[Cokleisli[Option, ?, ?]", SerializableTests.serializable(Split[Cokleisli[Option, ?, ?]]))

  {
    // Ceremony to help scalac to do the right thing, see also #267.
    type CokleisliNEL[A, B] = Cokleisli[NonEmptyList, A, B]

    implicit def ev0[A: Arbitrary, B: Arbitrary]: Arbitrary[CokleisliNEL[A, B]] =
      cokleisliArbitrary

    implicit def ev1[A: Arbitrary, B: Eq]: Eq[CokleisliNEL[A, B]] =
      cokleisliEq[NonEmptyList, A, B](oneAndArbitrary, Eq[B])

    checkAll("Cokleisli[NonEmptyList, Int, Int]", ArrowTests[CokleisliNEL].arrow[Int, Int, Int, Int, Int, Int])
    checkAll("Arrow[Cokleisli[NonEmptyList, ?, ?]]", SerializableTests.serializable(Arrow[CokleisliNEL]))
  }

  {
    // More ceremony, see above
    type CokleisliNELE[A] = Cokleisli[NonEmptyList, A, A]

    implicit def ev0[A: Arbitrary]: Arbitrary[CokleisliNELE[A]] =
      cokleisliArbitrary[NonEmptyList, A, A]

    implicit def ev1[A: Eq](implicit arb: Arbitrary[A]): Eq[CokleisliNELE[A]] =
      cokleisliEqE[NonEmptyList, A](oneAndArbitrary, Eq[A])

    {
      implicit val cokleisliMonoidK = Cokleisli.catsDataMonoidKForCokleisli[NonEmptyList]
      checkAll("Cokleisli[NonEmptyList, Int, Int]", MonoidKTests[CokleisliNELE].monoidK[Int])
      checkAll("MonoidK[Lambda[A => Cokleisli[NonEmptyList, A, A]]]", SerializableTests.serializable(cokleisliMonoidK))
    }

    {
      implicit val cokleisliSemigroupK = Cokleisli.catsDataSemigroupKForCokleisli[NonEmptyList]
      checkAll("Cokleisli[NonEmptyList, Int, Int]", SemigroupKTests[CokleisliNELE].semigroupK[Int])
      checkAll("SemigroupK[Lambda[A => Cokleisli[NonEmptyList, A, A]]]", SerializableTests.serializable(cokleisliSemigroupK))
    }

  }

  test("contramapValue with Id consistent with lmap"){
    forAll { (c: Cokleisli[Id, Int, Long], f: Char => Int) =>
      c.contramapValue[Char](f) should === (c.lmap(f))
    }
  }
}
