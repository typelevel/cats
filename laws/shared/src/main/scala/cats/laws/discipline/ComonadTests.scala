package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait ComonadTests[F[_]] extends CoflatMapTests[F] {
  def laws: ComonadLaws[F]

  def comonad[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqB: Eq[B],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]

    new DefaultRuleSet(
      name = "comonad",
      parent = Some(coflatMap[A, B, C]),
      "comonad left identity" -> forAll(laws.comonadLeftIdentity[A] _),
      "comonad right identity" -> forAll(laws.comonadRightIdentity[A, B] _))
  }
}

object ComonadTests {
  def apply[F[_]: Comonad]: ComonadTests[F] =
    new ComonadTests[F] { def laws: ComonadLaws[F] = ComonadLaws[F] }
}
