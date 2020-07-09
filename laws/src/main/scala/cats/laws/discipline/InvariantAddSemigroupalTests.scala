package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}

trait InvariantChoiceTests[F[_]] extends InvariantTests[F] {
  def laws: InvariantChoiceLaws[F]

  def invariantChoice[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[Either[A, Either[B, C]]]]
  ): RuleSet =
    new RuleSet {
      val name = "invariantChoice"
      val parents = Seq(invariant[A, B, C])
      val bases = Nil
      val props = Seq(
        "invariant additive associativity" -> forAll(laws.choiceAssociativity[A, B, C] _)
      )
    }
}

object InvariantChoiceTests {
  def apply[F[_]: InvariantChoice]: InvariantChoiceTests[F] =
    new InvariantChoiceTests[F] { def laws: InvariantChoiceLaws[F] = InvariantChoiceLaws[F] }
}
