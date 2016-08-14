package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait FunctorFilterTests[F[_]] extends FunctorTests[F] {
  def laws: FunctorFilterLaws[F]

  def functorFilter[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                        ArbFA: Arbitrary[F[A]],
                                                        ArbAOB: Arbitrary[A => Option[B]],
                                                        ArbBOC: Arbitrary[B => Option[C]],
                                                        ArbAB: Arbitrary[A => C],
                                                        EqFA: Eq[F[A]],
                                                        EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "functorFilter",
      parent = Some(functor[A, B, C]),
      "mapFilter composition" -> forAll(laws.mapFilterComposition[A, B, C] _),
      "mapFilter map consistency" -> forAll(laws.mapFilterMapConsistency[A, C] _))
  }
}

object FunctorFilterTests {
  def apply[F[_]: FunctorFilter]: FunctorFilterTests[F] =
    new FunctorFilterTests[F] { def laws: FunctorFilterLaws[F] = FunctorFilterLaws[F] }
}
