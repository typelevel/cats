package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait ApplyTests[F[_]] extends FunctorTests[F] {
  def laws: ApplyLaws[F]

  def apply[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit def ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]

    new DefaultRuleSet(
      name = "apply",
      parent = Some(functor[A, B, C]),
      "apply composition" -> forAll(laws.applyComposition[A, B, C] _))
  }
}

object ApplyTests {
  def apply[F[_]: Apply]: ApplyTests[F] =
    new ApplyTests[F] { def laws: ApplyLaws[F] = ApplyLaws[F] }
}
