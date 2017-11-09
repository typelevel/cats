package cats
package laws
package discipline

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws
import cats.instances.option._
import cats.instances.unit._

trait UnorderedFoldableTests[F[_]] extends Laws {
  def laws: UnorderedFoldableLaws[F]

  def unorderedFoldable[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFOptA: Arbitrary[F[Option[A]]],
    A: Monoid[A],
    B: Monoid[B],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    EqA: Eq[A],
    EqFA: Eq[F[A]],
    EqB: Eq[B],
    EqOptionA: Eq[Option[A]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "unorderedFoldable",
      parent = None,
      "forall true if empty" -> forAll(laws.forallEmpty[A] _),
      "exists false if empty" -> forAll(laws.existsEmpty[A] _),
      "size zero iff empty" -> forAll(laws.sizeEmpty[A] _),
      "forall consistent with exists" -> forAll(laws.forallConsistentWithExists[A] _),
      "sequenceUnordered_ consistent with traverseUnordered_ (Option)" ->
        forAll(laws.sequenceUnordered_IsTraverseUnordered_WithIdentity[Option, A] _),
      "exists is lazy" -> forAll(laws.existsLazy[A] _),
      "forall is lazy" -> forAll(laws.forallLazy[A] _),
      "exists consistent with findAny" -> forAll(laws.existsConsistentWithFindAny[A] _),
      "forall consistent with findAny" -> forAll(laws.forallConsistentWithFindAny[A] _),
    )
  }
}


object UnorderedFoldableTests {
  def apply[F[_]: UnorderedFoldable]: UnorderedFoldableTests[F] =
    new UnorderedFoldableTests[F] { def laws: UnorderedFoldableLaws[F] = UnorderedFoldableLaws[F] }
}
