package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait FoldableTests[F[_]] extends Laws {
  def foldable[A: Arbitrary]: RuleSet = {

    new DefaultRuleSet(
      name = "foldable",
      parent = None)
  }
}


object FoldableTests {
  def apply[F[_]: Foldable]: FoldableTests[F] =
    new FoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
