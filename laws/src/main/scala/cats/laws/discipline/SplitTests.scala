package cats.laws
package discipline

import cats.Eq
import cats.arrow.Split
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait SplitTests[F[_, _]] extends ComposeTests[F] {
  def laws: SplitLaws[F]

  def split[A, B, C, D, E, G](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    ArbFDE: Arbitrary[F[D, E]],
    ArbFEG: Arbitrary[F[E, G]],
    EqFAD: Eq[F[A, D]],
    EqFADCG: Eq[F[(A, D),(C, G)]]
  ): RuleSet =
    new RuleSet {
      def name = "split"
      def bases = Nil
      def parents = Seq(compose[A, B, C, D])
      def props = Seq(
        "split interchange" -> forAll(laws.splitInterchange[A, B, C, D, E, G] _)
      )
    }
}

object SplitTests {
  def apply[F[_, _]: Split]: SplitTests[F] =
    new SplitTests[F] { def laws = SplitLaws[F] }
}
