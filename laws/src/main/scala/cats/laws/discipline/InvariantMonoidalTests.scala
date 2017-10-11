package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait InvariantMonoidalTests[F[_]] extends InvariantTests[F] with SemigroupalTests[F] {
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
    EqFC: Eq[F[C]]
  ): RuleSet =
    new RuleSet {
      val name = "invariantMonoidal"
      val parents = Seq(invariant[A, B, C], semigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "invariant monoidal left identity" -> forAll((fa: F[A], b: B) => laws.invariantMonoidalLeftIdentity(fa, b)),
        "invariant monoidal right identity" -> forAll((fa: F[A], b: B) => laws.invariantMonoidalRightIdentity(fa, b)),
        "invariant monoidal associativity" -> forAll((fa: F[A], fb: F[B], fc: F[C]) => laws.invariantMonoidalAssociativity(fa, fb, fc))
      )
    }
}

object InvariantMonoidalTests {
  def apply[F[_]: InvariantMonoidal]: InvariantMonoidalTests[F] =
    new InvariantMonoidalTests[F] { def laws: InvariantMonoidalLaws[F] = InvariantMonoidalLaws[F] }
}
