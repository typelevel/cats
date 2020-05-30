package cats
package laws
package discipline

import cats.instances.option._
import cats.instances.long._
import cats.kernel.CommutativeMonoid
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll

trait ReducibleTests[F[_]] extends FoldableTests[F] {
  def laws: ReducibleLaws[F]

  def reducible[G[_]: Applicative, A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFGA: Arbitrary[F[G[A]]],
    ArbGB: Arbitrary[G[B]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    EqG: Eq[G[Unit]],
    EqA: Eq[A],
    EqB: Eq[B],
    EqFA: Eq[F[A]],
    EqOptionA: Eq[Option[A]],
    MonoidA: CommutativeMonoid[A],
    MonoidB: CommutativeMonoid[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "reducible",
      parent = Some(foldable[A, B]),
      "reduceLeftTo consistent with reduceMap" -> forAll(laws.reduceLeftToConsistentWithReduceMap[A, B] _),
      "reduceRightTo consistent with reduceMap" -> forAll(laws.reduceRightToConsistentWithReduceMap[A, B] _),
      "reduceRightTo consistent with reduceRightToOption" ->
        forAll(laws.reduceRightToConsistentWithReduceRightToOption[A, B] _),
      "reduceRight consistent with reduceRightOption" ->
        forAll(laws.reduceRightConsistentWithReduceRightOption[A] _),
      "reduce consistent with reduceLeft" ->
        forAll(laws.reduceReduceLeftConsistent[B] _),
      "nonEmptyTraverse_ consistent with traverse_" -> forAll(laws.traverseConsistent[G, A, B] _),
      "nonEmptySequence_ consistent with sequence_" -> forAll(laws.sequenceConsistent[G, A] _),
      "size consistent with reduceMap" -> forAll(laws.sizeConsistent[A] _)
    )
}

object ReducibleTests {
  def apply[F[_]: Reducible]: ReducibleTests[F] =
    new ReducibleTests[F] { def laws: ReducibleLaws[F] = ReducibleLaws[F] }
}
