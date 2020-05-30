package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait FunctorFilterTests[F[_]] extends Laws {
  def laws: FunctorFilterLaws[F]

  def functorFilter[A, B, C](implicit
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
  ): RuleSet =
    new DefaultRuleSet(
      name = "functorFilter",
      parent = None,
      "mapFilter composition" -> forAll(laws.mapFilterComposition[A, B, C] _),
      "mapFilter map consistency" -> forAll(laws.mapFilterMapConsistency[A, B] _),
      "collect mapFilter consistency" -> forAll(laws.collectConsistentWithMapFilter[A, B] _),
      "flattenOption mapFilter consistency" -> forAll(laws.flattenOptionConsistentWithMapFilter[A] _),
      "filter mapFilter consistency" -> forAll(laws.filterConsistentWithMapFilter[A] _),
      "filterNot mapFilter consistency" -> forAll(laws.filterNotConsistentWithFilter[A] _)
    )
}

object FunctorFilterTests {
  def apply[F[_]: FunctorFilter]: FunctorFilterTests[F] =
    new FunctorFilterTests[F] { def laws: FunctorFilterLaws[F] = FunctorFilterLaws[F] }
}
