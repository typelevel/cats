package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import org.typelevel.discipline.Laws
import cats.kernel.CommutativeMonoid
import cats.instances.set._
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
    EqFB: Eq[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "unorderedFoldable",
      parent = None,
      "unorderedFold consistent with unorderedFoldMap" -> forAll(laws.unorderedFoldConsistentWithUnorderedFoldMap[A] _),
      "forall consistent with exists" -> forAll(laws.forallConsistentWithExists[A] _),
      "forall true if empty" -> forAll(laws.forallEmpty[A] _),
      "nonEmpty reference" -> forAll(laws.nonEmptyRef[A] _)
    )
}

object UnorderedFoldableTests {
  def apply[F[_]: UnorderedFoldable]: UnorderedFoldableTests[F] =
    new UnorderedFoldableTests[F] { def laws: UnorderedFoldableLaws[F] = UnorderedFoldableLaws[F] }
}
