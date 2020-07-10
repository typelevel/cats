package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}

trait ContravariantChoosableTests[F[_]] extends ContravariantChoiceTests[F] with InvariantChoosableTests[F] {
  def laws: ContravariantChoosableLaws[F]

  def contravariantChoosable[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
      val name = "contravariantChoosable"
      val parents = Seq(contravariantChoice[A, B, C], invariantChoosable[A, B, C])
      val bases = Seq.empty
      val props = Seq.empty
    }
}

object ContravariantChoosableTests {
  def apply[F[_]: ContravariantChoosable]: ContravariantChoosableTests[F] =
    new ContravariantChoosableTests[F] { def laws: ContravariantChoosableLaws[F] = ContravariantChoosableLaws[F] }
}
