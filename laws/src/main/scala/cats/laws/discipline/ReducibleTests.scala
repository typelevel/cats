package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ReducibleTests[F[_]] extends FoldableTests[F] {
  def laws: ReducibleLaws[F]

  def reducible[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    B: Monoid[B],
    EqB: Eq[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "reducible",
      parent = Some(foldable[A, B]),
      "reduceLeftTo consistent with reduceMap" -> forAll(laws.reduceLeftToConsistentWithReduceMap[A, B] _),
      "reduceRightTo consistent with reduceMap" -> forAll(laws.reduceRightToConsistentWithReduceMap[A, B] _)
    )
}

object ReducibleTests {
  def apply[F[_] : Reducible]: ReducibleTests[F] =
    new ReducibleTests[F] { def laws: ReducibleLaws[F] = ReducibleLaws[F] }
}
