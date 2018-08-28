package cats
package laws
package discipline

import cats.data.Nested
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import cats.instances.option._

trait TraverseEmptyTests[F[_]] extends FunctorEmptyTests[F] {
  def laws: TraverseEmptyLaws[F]

  def traverseEmpty[A, B, C](implicit
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
      name = "traverseEmpty",
      parent = Some(functorEmpty[A, B, C]),
      "traverseFilter identity" -> forAll(laws.traverseFilterIdentity[Option, A] _),
      "traverseFilter nested composition" -> forAll(laws.traverseFilterComposition[A, B, C, Option, Option] _),
      "traverseFilter consistent with traverse" -> forAll(laws.traverseFilterConsistentWithTraverse[Option, A] _),
      "filterA consistent with traverseFilter" -> forAll(laws.filterAConsistentWithTraverseFilter[Option, A] _)
    )
  }
}

object TraverseEmptyTests {
  def apply[F[_]: TraverseEmpty]: TraverseEmptyTests[F] =
    new TraverseEmptyTests[F] { def laws: TraverseEmptyLaws[F] = TraverseEmptyLaws[F] }
}
