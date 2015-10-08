package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadReaderTests[F[_], R] extends MonadTests[F] {
  def laws: MonadReaderLaws[F, R]

  implicit def arbitraryK: ArbitraryK[F]
  implicit def eqK: EqK[F]

  def monadReader[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFR: Eq[F[R]],
    ArbE: Arbitrary[R]
  ): RuleSet = {
    implicit def ArbFRA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFRB: Arbitrary[F[B]] = ArbF.synthesize[B]

    new RuleSet {
      def name: String = "monadReader"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadReader ask idempotent" -> laws.monadReaderAskIdempotent,
        "monadReader local ask" -> forAll(laws.monadReaderLocalAsk _),
        "monadReader local pure" -> forAll(laws.monadReaderLocalPure[A] _),
        "monadReader local flatMap" -> forAll(laws.monadReaderLocalFlatMap[A, B] _)
      )
    }
  }
}

object MonadReaderTests {
  def apply[F[_], R](implicit FR: MonadReader[F, R], arbKFR: ArbitraryK[F], eqKFR: EqK[F]): MonadReaderTests[F, R] =
    new MonadReaderTests[F, R] {
      def arbitraryK: ArbitraryK[F] = arbKFR
      def eqK: EqK[F] = eqKFR
      def laws: MonadReaderLaws[F, R] = MonadReaderLaws[F, R]
    }
}
