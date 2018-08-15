package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait FunctorEmptyTests[F[_]] extends Laws {
  def laws: FunctorEmptyLaws[F]

  def functorEmpty[A, B, C](implicit
                            ArbFA: Arbitrary[F[A]],
                            ArbFABoo: Arbitrary[PartialFunction[A, B]],
                            ArbFOA: Arbitrary[F[Option[A]]],
                            ArbAOB: Arbitrary[A => Option[B]],
                            ArbBOC: Arbitrary[B => Option[C]],
                            ArbAB: Arbitrary[A => B],
                            ArbABoo: Arbitrary[A => Boolean],
                            EqFA: Eq[F[A]],
                            EqFB: Eq[F[B]],
                            EqFC: Eq[F[C]]
                           ): RuleSet = {
    new DefaultRuleSet(
      name = "functorEmpty",
      parent = None,
      "mapFilter composition" -> forAll(laws.mapFilterComposition[A, B, C] _),
      "mapFilter map consistency" -> forAll(laws.mapFilterMapConsistency[A, B] _),
      "collect mapFilter consistency" -> forAll(laws.collectConsistentWithMapFilter[A, B] _),
      "flattenOption mapFilter consistency" -> forAll(laws.flattenOptionConsistentWithMapFilter[A] _),
      "filter mapFilter consistency" -> forAll(laws.filterConsistentWithMapFilter[A] _)
    )
  }
}

object FunctorEmptyTests {
  def apply[F[_]: FunctorEmpty]: FunctorEmptyTests[F] =
    new FunctorEmptyTests[F] { def laws: FunctorEmptyLaws[F] = FunctorEmptyLaws[F] }
}
