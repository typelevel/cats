package cats.laws.discipline

import cats.laws.MonadCombineLaws
import cats.{MonadCombine, Eq}
import org.scalacheck.Arbitrary

trait MonadCombineTests[F[_]] extends MonadFilterTests[F] with MonoidKTests[F] {
  def laws: MonadCombineLaws[F]

  def monadCombine[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]

    new RuleSet {
      def name = "monadCombine"
      def bases = Nil
      def parents = Seq(monadFilter[A, B, C], monoidK[A])
      def props = Seq.empty // TODO
    }
  }
}

object MonadCombineTests {
  def apply[F[_]: MonadCombine]: MonadCombineTests[F] =
    new MonadCombineTests[F] { def laws = MonadCombineLaws[F] }
}
