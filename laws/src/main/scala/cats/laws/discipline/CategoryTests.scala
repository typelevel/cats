package cats
package laws
package discipline

import cats.arrow.Category
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait CategoryTests[F[_, _]] extends ComposeTests[F] {
  def laws: CategoryLaws[F]

  def category[A, B, C, D](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "category",
      parent = Some(compose[A, B, C, D]),
      "category left identity" -> forAll(laws.categoryLeftIdentity[A, B] _),
      "category right identity" -> forAll(laws.categoryRightIdentity[A, B] _)
    )
}

object CategoryTests {
  def apply[F[_, _]: Category]: CategoryTests[F] =
    new CategoryTests[F] { def laws: CategoryLaws[F] = CategoryLaws[F] }
}
