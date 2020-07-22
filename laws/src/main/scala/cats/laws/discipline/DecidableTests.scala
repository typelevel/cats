package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait DecidableTests[F[_]] extends ContravariantMonoidalTests[F] {
  def laws: DecidableLaws[F]

  def decidable[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    EqCC: Eq[F[Either[C, C]]],
    EqFEitABC: Eq[F[Either[Either[A, B], C]]],
    iso: SemigroupalTests.Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name = "decideable"
      val parents = Seq(contravariantMonoidal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "decideable right absorption" ->
          forAll(laws.decideableDecideRightAbsorption[A] _),
        "decideable sum associativity" ->
          forAll(laws.decideableSumAssociativity[A, B, C] _),
        "decideable right distributivity" ->
          forAll(laws.decideableRightDistributivity[A, B, C] _)
      )
    }
}

object DecidableTests {
  def apply[F[_]: Decidable]: DecidableTests[F] =
    new DecidableTests[F] { def laws: DecidableLaws[F] = DecidableLaws[F] }
}
