package cats.tests

import cats.{Contravariant, Id, Monad, MonoidK, SemigroupK, Semigroupal}
import cats.arrow._
import cats.data.{Cokleisli, NonEmptyList}
import cats.kernel.Eq
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.eq._
import org.scalacheck.Prop._
import org.scalacheck.Test.Parameters

class CokleisliSuite extends SlowCatsSuite {

  implicit override val scalaCheckTestParameters: Parameters =
    slowCheckConfiguration.withMinSuccessfulTests(20)

  implicit def cokleisliEq[F[_], A, B](implicit ev: Eq[F[A] => B]): Eq[Cokleisli[F, A, B]] =
    Eq.by[Cokleisli[F, A, B], F[A] => B](_.run)

  implicit val iso: Isomorphisms[Cokleisli[Option, Int, *]] = Isomorphisms.invariant[Cokleisli[Option, Int, *]]

  checkAll("Cokleisli[Option, MiniInt, Int]",
           SemigroupalTests[Cokleisli[Option, MiniInt, *]].semigroupal[Int, Int, Int]
  )
  checkAll("Semigroupal[Cokleisli[Option, Int, *]]",
           SerializableTests.serializable(Semigroupal[Cokleisli[Option, Int, *]])
  )

  checkAll("Cokleisli[Option, MiniInt, Int]", MonadTests[Cokleisli[Option, MiniInt, *]].monad[Int, Int, Int])
  checkAll("Monad[Cokleisli[Option, Int, *]]", SerializableTests.serializable(Monad[Cokleisli[Option, Int, *]]))

  checkAll("Cokleisli[Option, MiniInt, Int]",
           ProfunctorTests[Cokleisli[Option, *, *]].profunctor[MiniInt, Int, Int, Int, Int, Int]
  )
  checkAll("Profunctor[Cokleisli[Option, *, *]]", SerializableTests.serializable(Profunctor[Cokleisli[Option, *, *]]))

  checkAll("Cokleisli[Option, MiniInt, MiniInt]",
           ContravariantTests[Cokleisli[Option, *, MiniInt]].contravariant[MiniInt, MiniInt, MiniInt]
  )
  checkAll("Contravariant[Cokleisli[Option, *, MiniInt]]",
           SerializableTests.serializable(Contravariant[Cokleisli[Option, *, Int]])
  )

  checkAll("Cokleisli[(Boolean, *), MiniInt, MiniInt]",
           MonoidKTests[λ[α => Cokleisli[(Boolean, *), α, α]]].monoidK[MiniInt]
  )
  checkAll("MonoidK[λ[α => Cokleisli[NonEmptyList, α, α]]]",
           SerializableTests.serializable(MonoidK[λ[α => Cokleisli[NonEmptyList, α, α]]])
  )

  checkAll("Cokleisli[Option, MiniInt, MiniInt]", SemigroupKTests[λ[α => Cokleisli[Option, α, α]]].semigroupK[MiniInt])
  checkAll("SemigroupK[λ[α => Cokleisli[List, α, α]]]",
           SerializableTests.serializable(SemigroupK[λ[α => Cokleisli[List, α, α]]])
  )

  checkAll("Cokleisli[(Boolean, *), MiniInt, MiniInt]",
           ArrowTests[Cokleisli[(Boolean, *), *, *]].arrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )
  checkAll("Arrow[Cokleisli[NonEmptyList, *, *]]", SerializableTests.serializable(Arrow[Cokleisli[NonEmptyList, *, *]]))

  {
    implicit def cokleisliIdEq[A, B](implicit evidence: Eq[A => B]): Eq[Cokleisli[Id, A, B]] =
      Eq.by[Cokleisli[Id, A, B], A => B](_.run)

    checkAll(
      "Cokleisli[Id, MiniInt, MiniInt]",
      CommutativeArrowTests[Cokleisli[Id, *, *]].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
    )
    checkAll("CommutativeArrow[Cokleisli[Id, *, *]]",
             SerializableTests.serializable(CommutativeArrow[Cokleisli[Id, *, *]])
    )
  }

  test("contramapValue with Id consistent with lmap") {
    forAll { (c: Cokleisli[Id, Int, Long], f: MiniInt => Int) =>
      assert(c.contramapValue[MiniInt](f) === (c.lmap(f)))
    }
  }
}
