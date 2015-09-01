package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadFilterTests[F[_]] extends MonadTests[F] {
  def laws: MonadFilterLaws[F]

  def monadFilter[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]

    new DefaultRuleSet(
      name = "monadFilter",
      parent = Some(monad[A, B, C]),
      "monadFilter left empty" -> forAll(laws.monadFilterLeftEmpty[A, B] _),
      "monadFilter right empty" -> forAll(laws.monadFilterRightEmpty[A, B] _))
  }
}

object MonadFilterTests {
  def apply[F[_]: MonadFilter]: MonadFilterTests[F] =
    new MonadFilterTests[F] { def laws: MonadFilterLaws[F] = MonadFilterLaws[F] }
}
