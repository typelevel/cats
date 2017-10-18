package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import org.typelevel.discipline.Laws
import cats.kernel.CommutativeMonoid
import cats.instances.set._

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
      "foldLeft consistent with unorderedFoldMap" -> forAll(laws.foldLeftConsistentWithUnorderedFoldMap[A, B] _),
      "unorderedFold consistent with unorderedFoldMap" -> forAll(laws.unorderedFoldConsistentWithUnorderedFoldMap[A] _),
      "exists consistent with find" -> forAll(laws.existsConsistentWithFind[A] _),
      "forall consistent with exists" -> forAll(laws.forallConsistentWithExists[A] _),
      "forall true if empty" -> forAll(laws.forallEmpty[A] _),
      "toSet reference" -> forAll(laws.toSetRef[A] _)
    )
}

object UnorderedFoldableTests {
  def apply[F[_]: UnorderedFoldable]: UnorderedFoldableTests[F] =
    new UnorderedFoldableTests[F] { def laws: UnorderedFoldableLaws[F] = UnorderedFoldableLaws[F] }
}
