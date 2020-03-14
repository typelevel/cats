package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import org.typelevel.discipline.Laws
import cats.kernel.CommutativeMonoid
import cats.instances.boolean._

trait UnorderedFoldableTests[F[_]] extends Laws {
  def laws: UnorderedFoldableLaws[F]

  def unorderedFoldable[A: Arbitrary, B: Arbitrary](implicit
                                                    ArbFA: Arbitrary[F[A]],
                                                    ArbF: Arbitrary[A => B],
                                                    CogenA: Cogen[A],
                                                    A: CommutativeMonoid[A],
                                                    B: CommutativeMonoid[B],
                                                    EqFA: Eq[A],
                                                    EqFB: Eq[B],
                                                    EqOptionA: Eq[Option[A]]): RuleSet =
    new DefaultRuleSet(
      name = "unorderedFoldable",
      parent = None,
      "unorderedFold consistent with unorderedFoldMap" -> forAll(laws.unorderedFoldConsistentWithUnorderedFoldMap[A] _),
      "unorderedReduceOption consistent with unorderedFold" -> forAll(
        laws.unorderedReduceOptionConsistentWithUnorderedFold[A] _
      ),
      "forall consistent with exists" -> forAll(laws.forallConsistentWithExists[A] _),
      "forall true if empty" -> forAll(laws.forallEmpty[A] _),
      "nonEmpty reference" -> forAll(laws.nonEmptyRef[A] _),
      "exists is lazy" -> forAll(laws.existsLazy[A] _),
      "forall is lazy" -> forAll(laws.forallLazy[A] _)
    )
}

object UnorderedFoldableTests {
  def apply[F[_]: UnorderedFoldable]: UnorderedFoldableTests[F] =
    new UnorderedFoldableTests[F] { def laws: UnorderedFoldableLaws[F] = UnorderedFoldableLaws[F] }
}
