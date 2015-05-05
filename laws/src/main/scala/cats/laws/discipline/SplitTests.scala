package cats
package laws
package discipline

import cats.arrow.Split
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait SplitTests[F[_, _]] extends ComposeTests[F] {
  def laws: SplitLaws[F]

  def split[A, B, C, D, E, G](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    ArbFDE: Arbitrary[F[D, E]],
    ArbFEG: Arbitrary[F[E, G]],
    EqFAD: Eq[F[A, D]],
    EqFADCG: Eq[F[(A, D), (C, G)]]
  ): RuleSet =
    new RuleSet {
      def name: String = "split"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(compose[A, B, C, D])
      def props: Seq[(String, Prop)] = Seq(
        "split interchange" -> forAll(laws.splitInterchange[A, B, C, D, E, G] _)
      )
    }
}

object SplitTests {
  def apply[F[_, _]: Split]: SplitTests[F] =
    new SplitTests[F] { def laws: SplitLaws[F] = SplitLaws[F] }
}
