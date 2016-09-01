package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll

trait ReducibleTests[F[_]] extends FoldableTests[F] {
  def laws: ReducibleLaws[F]

  def reducible[G[_]: Applicative, A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFGA: Arbitrary[F[G[A]]],
    ArbGB: Arbitrary[G[B]],
    CogenA: Cogen[A],
    EqG: Eq[G[Unit]],
    EqB: Eq[B],
    MonoidB: Monoid[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "reducible",
      parent = Some(foldable[A, B]),
      "reduceLeftTo consistent with reduceMap" -> forAll(laws.reduceLeftToConsistentWithReduceMap[A, B] _),
      "reduceRightTo consistent with reduceMap" -> forAll(laws.reduceRightToConsistentWithReduceMap[A, B] _),
      "traverse1_ consistent with traverse_" -> forAll(laws.traverseConsistent[G, A, B] _),
      "sequence1_ consistent with sequence_" -> forAll(laws.sequenceConsistent[G, A] _)
    )
}

object ReducibleTests {
  def apply[F[_] : Reducible]: ReducibleTests[F] =
    new ReducibleTests[F] { def laws: ReducibleLaws[F] = ReducibleLaws[F] }
}
