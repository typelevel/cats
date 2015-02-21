package cats.laws
package discipline

import cats.Eq
import cats.arrow.Compose
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait ComposeTests[F[_, _]] extends Laws {
  def laws: ComposeLaws[F]

  def compose[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary](implicit
    ArbF: ArbitraryK2[F],
    EqFAD: Eq[F[A, D]]
  ): RuleSet = {
    implicit val ArbFAB = ArbF.synthesize[A, B]
    implicit val ArbFBC = ArbF.synthesize[B, C]
    implicit val ArbFCD = ArbF.synthesize[C, D]

    new RuleSet {
      def name = "compose"
      def bases = Nil
      def parents = Nil
      def props = Seq(
        "compose associativity" -> forAll(laws.composeAssociativity[A, B, C, D] _)
      )
    }
  }
}

object ComposeTests {
  def apply[F[_, _]: Compose]: ComposeTests[F] =
    new ComposeTests[F] { def laws = ComposeLaws[F] }
}
