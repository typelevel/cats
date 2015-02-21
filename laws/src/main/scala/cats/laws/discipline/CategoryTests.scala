package cats.laws
package discipline

import cats.Eq
import cats.arrow.Category
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait CategoryTests[F[_, _]] extends ComposeTests[F] {
  def laws: CategoryLaws[F]

  def category[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary](implicit
    ArbF: ArbitraryK2[F],
    EqFAD: Eq[F[A, D]]
  ): RuleSet = {
    implicit val ArbFAD = ArbF.synthesize[A, D]

    new RuleSet {
      def name = "category"
      def bases = Nil
      def parents = Seq(compose[A, B, C, D])
      def props = Seq(
        "category left identity" -> forAll(laws.categoryLeftIdentity[A, D] _),
        "category right identity" -> forAll(laws.categoryRightIdentity[A, D] _)
      )
    }
  }
}

object CategoryTests {
  def apply[F[_, _]: Category]: CategoryTests[F] =
    new CategoryTests[F] { def laws = CategoryLaws[F] }
}
