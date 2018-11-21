package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait InvariantAddMonoidalTests[F[_]] extends InvariantAddSemigroupalTests[F] {
  def laws: InvariantAddMonoidalLaws[F]

  def invariantAddMonoidal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                                     arbFA: Arbitrary[F[A]],
                                                                     arbFB: Arbitrary[F[B]],
                                                                     arbFC: Arbitrary[F[C]],
                                                                     CogenA: Cogen[A],
                                                                     CogenB: Cogen[B],
                                                                     CogenC: Cogen[C],
                                                                     EqFA: Eq[F[A]],
                                                                     EqFB: Eq[F[B]],
                                                                     EqFC: Eq[F[C]],
                                                                     EqFABC: Eq[F[Either[A, Either[B, C]]]]): RuleSet =
    new RuleSet {
      val name = "invariantAddMonoidal"
      val parents = Seq(invariantAddSemigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "invariant additive monoidal left identity" -> forAll(laws.sumEmptyLeftIdentity[A, B] _),
        "invariant additive monoidal right identity" -> forAll(laws.sumEmptyRightIdentity[A, B] _)
      )
    }
}

object InvariantAddMonoidalTests {
  def apply[F[_]: InvariantAddMonoidal]: InvariantAddMonoidalTests[F] =
    new InvariantAddMonoidalTests[F] { def laws: InvariantAddMonoidalLaws[F] = InvariantAddMonoidalLaws[F] }
}
