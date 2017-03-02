package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait FoldableTests[F[_]] extends Laws {
  def laws: FoldableLaws[F]

  def foldable[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    B: Monoid[B],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    EqB: Eq[B],
    EqOptionA: Eq[Option[A]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "foldable",
      parent = None,
      "foldLeft consistent with foldMap" -> forAll(laws.leftFoldConsistentWithFoldMap[A, B] _),
      "foldRight consistent with foldMap" -> forAll(laws.rightFoldConsistentWithFoldMap[A, B] _),
      "exists consistent with find" -> forAll(laws.existsConsistentWithFind[A] _),
      "forall consistent with exists" -> forAll(laws.forallConsistentWithExists[A] _),
      "forall true if empty" -> forAll(laws.forallEmpty[A] _),
      "exists is lazy" -> forAll(laws.existsLazy[A] _),
      "forall is lazy" -> forAll(laws.forallLazy[A] _),
      "foldM identity" -> forAll(laws.foldMIdentity[A, B] _),
      "reduceLeftOption consistent with reduceLeftToOption" ->
        forAll(laws.reduceLeftOptionConsistentWithReduceLeftToOption[A] _),
      "reduceRightOption consistent with reduceRightToOption" ->
        forAll(laws.reduceRightOptionConsistentWithReduceRightToOption[A] _)
    )
  }
}


object FoldableTests {
  def apply[F[_]: Foldable]: FoldableTests[F] =
    new FoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
