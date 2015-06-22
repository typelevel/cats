package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait BimonadTests[F[_]] extends MonadTests[F] with ComonadTests[F] {
  def laws: BimonadLaws[F]

  def bimonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet =
    new RuleSet {
      def name: String = "bimonad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], comonad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "pure and extract compose" -> forAll(laws.pureExtractComposition[A] _)
      )
    }
}

object BimonadTests {
  def apply[F[_]: Bimonad: ArbitraryK: EqK]: BimonadTests[F] =
    new BimonadTests[F] {
      def arbitraryK: ArbitraryK[F] = implicitly
      def eqK: EqK[F] = implicitly
      def laws: BimonadLaws[F] = BimonadLaws[F]
    }
}
