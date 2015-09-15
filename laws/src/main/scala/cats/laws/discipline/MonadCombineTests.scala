package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadCombineTests[F[_]] extends MonadFilterTests[F] with AlternativeTests[F] {
  def laws: MonadCombineLaws[F]

  def monadCombine[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbitraryK[F].synthesize
    implicit def ArbFB: Arbitrary[F[B]] = ArbitraryK[F].synthesize
    implicit def ArbFAB: Arbitrary[F[A => B]] = ArbitraryK[F].synthesize
    implicit def EqFA: Eq[F[A]] = EqK[F].synthesize
    implicit def EqFB: Eq[F[B]] = EqK[F].synthesize
    implicit def EqFC: Eq[F[C]] = EqK[F].synthesize

    new RuleSet {
      def name: String = "monadCombine"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monadFilter[A, B, C], alternative[A,B,C])
      def props: Seq[(String, Prop)] = Seq(
        "monadCombine left distributivity" -> forAll(laws.monadCombineLeftDistributivity[A, B] _)
      )
    }
  }
}

object MonadCombineTests {
  def apply[F[_]: MonadCombine: ArbitraryK: EqK]: MonadCombineTests[F] =
    new MonadCombineTests[F] {
      def arbitraryK: ArbitraryK[F] = implicitly
      def eqK: EqK[F] = implicitly
      def laws: MonadCombineLaws[F] = MonadCombineLaws[F]
    }
}
