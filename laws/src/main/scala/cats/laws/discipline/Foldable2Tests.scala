package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait Foldable2Tests[F[_, _]] extends Laws {
  def laws: Foldable2Laws[F]

  def foldable2[A: Arbitrary, B: Arbitrary, C: Arbitrary: Monoid: Eq](implicit
    ArbFAB: Arbitrary[F[A, B]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "foldable2",
      parent = None,
      "fold2Left consistent with fold2Map" -> forAll(laws.fold2LeftConsistentWithFold2Map[A, B, C] _),
      "fold2Right consistent with fold2Map" -> forAll(laws.fold2RightConsistentWithFold2Map[A, B, C] _)
    )
}

object Foldable2Tests {
  def apply[F[_, _]: Foldable2]: Foldable2Tests[F] =
    new Foldable2Tests[F] { def laws: Foldable2Laws[F] = Foldable2Laws[F] }
}
