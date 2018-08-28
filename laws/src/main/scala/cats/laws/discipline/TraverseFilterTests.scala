package cats
package laws
package discipline

import cats.data.Nested
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import cats.instances.option._

trait TraverseFilterTests[F[_]] extends FunctorFilterTests[F] {
  def laws: TraverseFilterLaws[F]

  def traverseFilter[A, B, C](implicit
                             ArbFA: Arbitrary[F[A]],
                             ArbFOA: Arbitrary[F[Option[A]]],
                             ArbFABoo: Arbitrary[PartialFunction[A, B]],
                             ArbAOB: Arbitrary[A => Option[B]],
                             ArbAOA: Arbitrary[A => Option[A]],
                             ArbAOOB: Arbitrary[A => Option[Option[B]]],
                             ArbBOC: Arbitrary[B => Option[C]],
                             ArbBOOC: Arbitrary[B => Option[Option[C]]],
                             ArbAB: Arbitrary[A => B],
                             ArbABoo: Arbitrary[A => Boolean],
                             ArbAOBoo: Arbitrary[A => Option[Boolean]],
                             EqFA: Eq[F[A]],
                             EqFB: Eq[F[B]],
                             EqFC: Eq[F[C]],
                             EqGFA: Eq[Option[F[A]]],
                             EqMNFC: Eq[Nested[Option, Option, F[C]]]
                            ): RuleSet = {
    new DefaultRuleSet(
      name = "traverseFilter",
      parent = Some(functorFilter[A, B, C]),
      "traverseFilter identity" -> forAll(laws.traverseFilterIdentity[Option, A] _),
      "traverseFilter nested composition" -> forAll(laws.traverseFilterComposition[A, B, C, Option, Option] _),
      "traverseFilter consistent with traverse" -> forAll(laws.traverseFilterConsistentWithTraverse[Option, A] _),
      "filterA consistent with traverseFilter" -> forAll(laws.filterAConsistentWithTraverseFilter[Option, A] _)
    )
  }
}

object TraverseFilterTests {
  def apply[F[_]: TraverseFilter]: TraverseFilterTests[F] =
    new TraverseFilterTests[F] { def laws: TraverseFilterLaws[F] = TraverseFilterLaws[F] }
}
