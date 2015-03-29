package cats.laws
package discipline

import cats.Eq
import cats.arrow.Compose
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait ComposeTests[F[_, _]] extends Laws {
  def laws: ComposeLaws[F]

  def compose[A, B, C, D](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAD: Eq[F[A, D]]
  ): RuleSet =
    new RuleSet {
      def name = "compose"
      def bases = Nil
      def parents = Nil
      def props = Seq(
        "compose associativity" -> forAll(laws.composeAssociativity[A, B, C, D] _)
      )
    }
}

object ComposeTests {
  def apply[F[_, _]: Compose]: ComposeTests[F] =
    new ComposeTests[F] { def laws = ComposeLaws[F] }
}
