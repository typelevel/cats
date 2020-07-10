package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}

trait ContravariantChoiceTests[F[_]] extends ContravariantTests[F] with InvariantChoiceTests[F] {
  def laws: ContravariantChoiceLaws[F]

  def contravariantChoice[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
      val name = "contravariantChoice"
      val parents = Seq(contravariant[A, B, C], invariantChoice[A, B, C])
      val bases = Nil
      val props = Seq(
        "contramap choice right distributivity" -> forAll(laws.contramapChoiceRightDistributivity[A, B, C] _)
      )
    }
}

object ContravariantChoiceTests {
  def apply[F[_]: ContravariantChoice]: ContravariantChoiceTests[F] =
    new ContravariantChoiceTests[F] {
      def laws: ContravariantChoiceLaws[F] = ContravariantChoiceLaws[F]
    }
}
