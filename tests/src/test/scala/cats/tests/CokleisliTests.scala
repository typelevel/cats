package cats
package tests

import cats.arrow.{Arrow, Split}
import cats.data.{Cokleisli, NonEmptyList}
import cats.functor.{Contravariant, Profunctor}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

class CokleisliTests extends SlowCatsSuite {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    slowCheckConfiguration.copy(
      sizeRange = slowCheckConfiguration.sizeRange.min(5),
      minSuccessful = slowCheckConfiguration.minSuccessful.min(20))

  implicit def cokleisliEq[F[_], A, B](implicit A: Arbitrary[F[A]], FB: Eq[B]): Eq[Cokleisli[F, A, B]] =
    Eq.by[Cokleisli[F, A, B], F[A] => B](_.run)

  def cokleisliEqE[F[_], A](implicit A: Arbitrary[F[A]], FA: Eq[A]): Eq[Cokleisli[F, A, A]] =
    Eq.by[Cokleisli[F, A, A], F[A] => A](_.run)

  implicit val iso = CartesianTests.Isomorphisms.invariant[Cokleisli[Option, Int, ?]]

  checkAll("Cokleisli[Option, Int, Int]", CartesianTests[Cokleisli[Option, Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Cokleisli[Option, Int, ?]]", SerializableTests.serializable(Cartesian[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", MonadTests[Cokleisli[Option, Int, ?]].monad[Int, Int, Int])
  checkAll("Monad[Cokleisli[Option, Int, ?]]", SerializableTests.serializable(Monad[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ProfunctorTests[Cokleisli[Option, ?, ?]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Profunctor[Cokleisli[Option, ?, ?]]", SerializableTests.serializable(Profunctor[Cokleisli[Option, ?, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", SplitTests[Cokleisli[Option, ?, ?]].split[Int, Int, Int, Int, Int, Int])
  checkAll("Split[Cokleisli[Option, ?, ?]]", SerializableTests.serializable(Split[Cokleisli[Option, ?, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ContravariantTests[Cokleisli[Option, ?, Int]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Cokleisli[Option, ?, Int]]", SerializableTests.serializable(Contravariant[Cokleisli[Option, ?, Int]]))

  checkAll("Cokleisli[NonEmptyList, Int, Int]", ArrowTests[Cokleisli[NonEmptyList, ?, ?]].arrow[Int, Int, Int, Int, Int, Int])
  checkAll("Arrow[Cokleisli[NonEmptyList, ?, ?]]", SerializableTests.serializable(Arrow[Cokleisli[NonEmptyList, ?, ?]]))

  checkAll("Cokleisli[NonEmptyList, Int, Int]", MonoidKTests[λ[α => Cokleisli[NonEmptyList, α, α]]].monoidK[Int])
  checkAll("MonoidK[λ[α => Cokleisli[NonEmptyList, α, α]]]", SerializableTests.serializable(MonoidK[λ[α => Cokleisli[NonEmptyList, α, α]]]))

  checkAll("Cokleisli[List, Int, Int]", SemigroupKTests[λ[α => Cokleisli[List, α, α]]].semigroupK[Int])
  checkAll("SemigroupK[λ[α => Cokleisli[List, α, α]]]", SerializableTests.serializable(SemigroupK[λ[α => Cokleisli[List, α, α]]]))

  test("contramapValue with Id consistent with lmap"){
    forAll { (c: Cokleisli[Id, Int, Long], f: Char => Int) =>
      c.contramapValue[Char](f) should === (c.lmap(f))
    }
  }
}
