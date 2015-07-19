package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadReaderTests[F[_, _], R] extends MonadTests[F[R, ?]] {
  def laws: MonadReaderLaws[F, R]

  def monadReader[A : Arbitrary, B : Arbitrary, C : Arbitrary](implicit
    ArbF: ArbitraryK[F[R, ?]],
    EqFA: Eq[F[R, A]],
    EqFB: Eq[F[R, B]],
    EqFC: Eq[F[R, C]],
    EqFR: Eq[F[R, R]],
    ArbE: Arbitrary[R]
  ): RuleSet = {
    implicit def ArbFRA: Arbitrary[F[R, A]] = ArbF.synthesize[A]
    implicit def ArbFRB: Arbitrary[F[R, B]] = ArbF.synthesize[B]

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
  def apply[F[_, _], R](implicit FR: MonadReader[F, R]): MonadReaderTests[F, R] =
    new MonadReaderTests[F, R] { def laws: MonadReaderLaws[F, R] = MonadReaderLaws[F, R] }
}
