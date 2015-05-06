package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadCombineTests[F[_]] extends MonadFilterTests[F] with AlternativeTests[F] {
  def laws: MonadCombineLaws[F]

  def monadCombine[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    arbFAB: Arbitrary[F[A => B]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]

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
  def apply[F[_]: MonadCombine]: MonadCombineTests[F] =
    new MonadCombineTests[F] { def laws: MonadCombineLaws[F] = MonadCombineLaws[F] }
}
