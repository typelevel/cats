package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait FoldableTests[F[_]] extends Laws {
  def laws: FoldableLaws[F]

  def foldable[A: Arbitrary, B: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    B: Monoid[B],
    EqB: Eq[B]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]

    new DefaultRuleSet(
      name = "foldable",
      parent = None,
      "foldLeft consistent with foldMap" -> forAll(laws.leftFoldConsistentWithFoldMap[A, B] _),
      "foldRight consistent with foldMap" -> forAll(laws.rightFoldConsistentWithFoldMap[A, B] _)
    )
  }
}


object FoldableTests {
  def apply[F[_]: Foldable]: FoldableTests[F] =
    new FoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
