package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}

trait ContravariantAddMonoidalTests[F[_]]
    extends ContravariantAddSemigroupalTests[F]
    with InvariantAddMonoidalTests[F] {
  def laws: ContravariantAddMonoidalLaws[F]

  def contravariantAddMonoidal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[Either[A, Either[B, C]]]],
    EqFECC: Eq[F[Either[C, C]]]
  ): RuleSet =
    new RuleSet {
      val name = "contravariantAddMonoidal"
      val parents = Seq(contravariantAddSemigroupal[A, B, C], invariantAddMonoidal[A, B, C])
      val bases = Seq.empty
      val props = Seq.empty
    }
}

object ContravariantAddMonoidalTests {
  def apply[F[_]: ContravariantAddMonoidal]: ContravariantAddMonoidalTests[F] =
    new ContravariantAddMonoidalTests[F] { def laws: ContravariantAddMonoidalLaws[F] = ContravariantAddMonoidalLaws[F] }
}
