package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadTests[F[_]] extends ApplicativeTests[F] with FlatMapTests[F] {
  def laws: MonadLaws[F]

  implicit def arbitraryK: ArbitraryK[F]
  implicit def eqK: EqK[F]

  def monad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbitraryK[F].synthesize
    implicit def ArbFB: Arbitrary[F[B]] = ArbitraryK[F].synthesize
    implicit val eqfa: Eq[F[A]] = EqK[F].synthesize
    implicit val eqfb: Eq[F[B]] = EqK[F].synthesize
    implicit val eqfc: Eq[F[C]] = EqK[F].synthesize

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
  def apply[F[_]: Monad: ArbitraryK: EqK]: MonadTests[F] =
    new MonadTests[F] {
      def arbitraryK: ArbitraryK[F] = implicitly
      def eqK: EqK[F] = implicitly
      def laws: MonadLaws[F] = MonadLaws[F]
    }
}
