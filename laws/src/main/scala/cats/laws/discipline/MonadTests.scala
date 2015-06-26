package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadTests[F[_]] extends ApplicativeTests[F] with FlatMapTests[F] {
  def laws: MonadLaws[F]

  def monad[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]

    new RuleSet {
      def name: String = "monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monad left identity" -> forAll(laws.monadLeftIdentity[A, B] _),
        "monad right identity" -> forAll(laws.monadRightIdentity[A] _)
      )
    }
  }
}

object MonadTests {
  def apply[F[_]: Monad]: MonadTests[F] =
    new MonadTests[F] { def laws: MonadLaws[F] = MonadLaws[F] }
}
