package cats
package laws
package discipline

import cats.laws.AlternativeLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait AlternativeTests[F[_]] extends ApplicativeTests[F] with MonoidKTests[F]  {
  def laws:AlternativeLaws[F]

  def alternative[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    arbFAB: Arbitrary[F[A => B]],
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
        "left distributivity" -> forAll((fa1: F[A], fa2: F[A], fab: A => B) =>
          laws.alternativeLeftDistributivity[A,B](fa1, fa2, fab)),
        "right distributivity" -> forAll((fa: F[A], fab1: F[A => B], fab2: F[A => B]) =>
          laws.alternativeRightDistributivity[A,B](fa, fab1, fab2)),
        "right absorption" -> forAll((fab: F[A => B]) =>
          laws.alternativeRightAbsorption[A,B](fab))
      )
    }
}

}

object AlternativeTests {
  def apply[F[_]: Alternative]: AlternativeTests[F] =
    new AlternativeTests[F] { def laws: AlternativeLaws[F] = AlternativeLaws[F] }
}
