package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait FunctorFlattenTests[F[_]] extends FunctorFilterTests[F] {
  def laws: FunctorFlattenLaws[F]

  def functorFlatten[A: Arbitrary, B: Arbitrary, C: Arbitrary, G1[_]: Foldable: Functor, G2[_]: Foldable](implicit
                                                        ArbFA: Arbitrary[F[A]],
                                                        ArbAOB: Arbitrary[A => Option[B]],
                                                        ArbBOC: Arbitrary[B => Option[C]],
                                                        ArbAG1B: Arbitrary[A => G1[B]],
                                                        ArbBG2C: Arbitrary[B => G2[C]],
                                                        ArbAB: Arbitrary[A => C],
                                                        EqFA: Eq[F[A]],
                                                        EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "functorFlatten",
      parent = Some(functorFilter[A, B, C]),
      "mapFlatten composition" -> forAll(laws.mapFlattenComposition[G1, G2, A, B, C] _),
      "mapFlatten map consistency" -> forAll(laws.mapFlattenMapConsistency[A, C] _))
  }
}

object FunctorFlattenTests {
  def apply[F[_]: FunctorFlatten]: FunctorFlattenTests[F] =
    new FunctorFlattenTests[F] { def laws: FunctorFlattenLaws[F] = FunctorFlattenLaws[F] }
}
