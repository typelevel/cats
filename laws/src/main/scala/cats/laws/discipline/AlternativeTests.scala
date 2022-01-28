package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait AlternativeTests[F[_]] extends NonEmptyAlternativeTests[F] with MonoidKTests[F] {
  def laws: AlternativeLaws[F]

  def alternative[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbIterableOnce: Arbitrary[IterableOnce[A]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name: String = "alternative"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(monoidK[A], nonEmptyAlternative[A, B, C])
      val props: Seq[(String, Prop)] = Seq(
        "right absorption" -> forAll(laws.alternativeRightAbsorption[A, B] _),
        "fromIterableOnce" -> forAll(laws.fromIterableOnce[A] _)
      )
    }
}

object AlternativeTests {
  def apply[F[_]: Alternative]: AlternativeTests[F] =
    new AlternativeTests[F] { def laws: AlternativeLaws[F] = AlternativeLaws[F] }

  def composed[F[_]: Alternative, G[_]: Applicative]: AlternativeTests[λ[α => F[G[α]]]] =
    new AlternativeTests[λ[α => F[G[α]]]] {
      def laws: AlternativeLaws[λ[α => F[G[α]]]] = AlternativeLaws.composed[F, G]
    }
}
