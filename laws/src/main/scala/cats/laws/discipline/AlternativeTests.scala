package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait AlternativeTests[F[_]] extends ApplicativeTests[F] with MonoidKTests[F]  {
  def laws: AlternativeLaws[F]

  def alternative[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    ArbFAB: Arbitrary[F[A => B]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]

    new RuleSet {
      val name: String = "alternative"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(monoidK[A], applicative[A,B,C])
      val props: Seq[(String, Prop)] = Seq(
        "left distributivity" -> forAll(laws.alternativeLeftDistributivity[A, B] _),
        "right distributivity" -> forAll(laws.alternativeRightDistributivity[A, B] _),
        "right absorption" -> forAll(laws.alternativeRightAbsorption[A, B] _)
      )
    }
}

}

object AlternativeTests {
  def apply[F[_]: Alternative]: AlternativeTests[F] =
    new AlternativeTests[F] { def laws: AlternativeLaws[F] = AlternativeLaws[F] }
}
