package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait BifoldableTests[F[_, _]] extends Laws {
  def laws: BifoldableLaws[F]

  def bifoldable[A: Arbitrary, B: Arbitrary, C: Arbitrary: Monoid: Eq](implicit
    ArbFAB: Arbitrary[F[A, B]],
    CogenA: Cogen[A],
    CogenB: Cogen[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "bifoldable",
      parent = None,
      "bifoldLeft consistent with bifoldMap" -> forAll(laws.bifoldLeftConsistentWithBifoldMap[A, B, C] _),
      "bifoldRight consistent with bifoldMap" -> forAll(laws.bifoldRightConsistentWithBifoldMap[A, B, C] _)
    )
}

object BifoldableTests {
  def apply[F[_, _]: Bifoldable]: BifoldableTests[F] =
    new BifoldableTests[F] { def laws: BifoldableLaws[F] = BifoldableLaws[F] }
}
