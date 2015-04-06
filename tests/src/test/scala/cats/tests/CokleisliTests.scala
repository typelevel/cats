package cats.tests

import cats.{Applicative, Eq}
import cats.data.{Cokleisli, NonEmptyList}
import cats.functor.{Profunctor, Strong}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

class CokleisliTests extends CatsSuite {

  implicit def cokleisliEq[F[_], A, B](implicit A: Arbitrary[F[A]], FB: Eq[B]): Eq[Cokleisli[F, A, B]] =
    Eq.by[Cokleisli[F, A, B], F[A] => B](_.run)

  checkAll("Cokleisli[Option, Int, Int]", ApplicativeTests[Cokleisli[Option, Int, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Cokleisli[Option, Int, ?]", SerializableTests.serializable(Applicative[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ProfunctorTests[Cokleisli[Option, ?, ?]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Profunctor[Cokleisli[Option, ?, ?]", SerializableTests.serializable(Profunctor[Cokleisli[Option, ?, ?]]))

  {
    // Ceremony to help scalac to do the right thing, see also #267.
    type CokleisliNEL[A, B] = Cokleisli[NonEmptyList, A, B]

    implicit def ev0[A: Arbitrary, B: Arbitrary]: Arbitrary[CokleisliNEL[A, B]] =
      cokleisliArbitrary

    implicit def ev1[A: Arbitrary, B: Eq]: Eq[CokleisliNEL[A, B]] =
      cokleisliEq[NonEmptyList, A, B](oneAndArbitrary, Eq[B])

    checkAll("Cokleisli[NonEmptyList, Int, Int]", StrongTests[CokleisliNEL].strong[Int, Int, Int, Int, Int, Int])
    checkAll("Strong[Cokleisli[NonEmptyList, ?, ?]]", SerializableTests.serializable(Strong[CokleisliNEL]))
  }
}
