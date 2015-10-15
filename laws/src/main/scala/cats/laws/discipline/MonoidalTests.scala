package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait MonoidalTests[F[_]] extends Laws {
  def laws: MonoidalLaws[F]

  def monoidal[A : Arbitrary, B : Arbitrary, C : Arbitrary](implicit ArbF: ArbitraryK[F], EqFA: Eq[F[A]], EqFB: Eq[F[B]], EqFC: Eq[F[C]]): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]
    implicit def ArbFC: Arbitrary[F[C]] = ArbF.synthesize[C]
    new DefaultRuleSet(
      name = "monoidal",
      parent = None,
      "invariant associativity" -> forAll(laws.covariantProductAssociativity[A, B, C] _)
    )
  }
}

object MonoidalTests {
  def apply[F[_] : Monoidal : Functor]: MonoidalTests[F] =
    new MonoidalTests[F] { def laws: MonoidalLaws[F] = MonoidalLaws.covariant[F] }
}