package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

import cats.instances.list._

trait FoldableTests[F[_]] extends Laws with UnorderedFoldableTests[F] {
  def laws: FoldableLaws[F]

  def foldable[A: Arbitrary, B: Arbitrary](implicit
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
      name = "foldable",
      parent = Some(unorderedFoldable[A, B]),
      "foldLeft consistent with foldMap" -> forAll(laws.leftFoldConsistentWithFoldMap[A, B] _),
      "foldRight consistent with foldMap" -> forAll(laws.rightFoldConsistentWithFoldMap[A, B] _),
      "ordered constistency" -> forAll(laws.orderedConsistency[A] _),
      "exists consistent with find" -> forAll(laws.existsConsistentWithFind[A] _),
      "forall consistent with find" -> forAll(laws.forallConsistentWithFind[A] _),
      "foldM identity" -> forAll(laws.foldMIdentity[A, B] _),
      "reduceLeftOption consistent with reduceLeftToOption" ->
        forAll(laws.reduceLeftOptionConsistentWithReduceLeftToOption[A] _),
      "reduceRightOption consistent with reduceRightToOption" ->
        forAll(laws.reduceRightOptionConsistentWithReduceRightToOption[A] _),
      "get reference" -> forAll(laws.getRef[A] _),
      "fold reference" -> forAll(laws.foldRef[A] _),
      "toList reference" -> forAll(laws.toListRef[A] _),
      "filter_ reference" -> forAll(laws.filter_Ref[A] _),
      "takeWhile_ reference" -> forAll(laws.takeWhile_Ref[A] _),
      "dropWhile_ reference" -> forAll(laws.dropWhile_Ref[A] _)
    )
  }
}


object FoldableTests {
  def apply[F[_]: Foldable]: FoldableTests[F] =
    new FoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
