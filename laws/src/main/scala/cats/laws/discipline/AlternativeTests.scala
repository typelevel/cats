package cats.laws
package discipline

import cats.{Alternative, Eq}
import cats.laws.AlternativeLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

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
      val name = "alternative"
      val bases = Nil
      val parents = Seq(monoidK[A], applicative[A,B,C])
      val props = Seq(
        "left distributivity" -> forAll(laws.alternativeLeftDistributivity[A,B](_, _, _)),
        "right distributivity" -> forAll(laws.alternativeRightDistributivity[A,B](_, _, _)),
        "right absorption" -> forAll(laws.alternativeRightAbsorption[A,B](_))
      )
    }
}

}

object AlternativeTests {
  def apply[F[_]: Alternative]: AlternativeTests[F] =
    new AlternativeTests[F] { def laws = AlternativeLaws[F] }
}
