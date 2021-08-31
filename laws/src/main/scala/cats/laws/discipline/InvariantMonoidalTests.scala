package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait InvariantMonoidalTests[F[_]] extends InvariantSemigroupalTests[F] {
  def laws: InvariantMonoidalLaws[F]

  def invariantMonoidal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFABC: Eq[F[(A, (B, C))]],
    EqFABC2: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet =
    new RuleSet {
      val name = "invariantMonoidal"
      val parents = Seq(invariantSemigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "invariant monoidal left identity" -> forAll((fa: F[A]) => laws.invariantMonoidalLeftIdentity(fa)),
        "invariant monoidal right identity" -> forAll((fa: F[A]) => laws.invariantMonoidalRightIdentity(fa))
      )
    }
}

object InvariantMonoidalTests {
  def apply[F[_]: InvariantMonoidal]: InvariantMonoidalTests[F] =
    new InvariantMonoidalTests[F] { def laws: InvariantMonoidalLaws[F] = InvariantMonoidalLaws[F] }
}
