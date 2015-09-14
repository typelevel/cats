package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadStateTests[F[_, _], S] extends MonadTests[F[S, ?]] {
  def laws: MonadStateLaws[F, S]

  implicit def arbitraryK: ArbitraryK[F[S, ?]]
  implicit def eqK: EqK[F[S, ?]]

  def monadState[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbF: ArbitraryK[F[S, ?]],
    EqFA: Eq[F[S, A]],
    EqFB: Eq[F[S, B]],
    EqFC: Eq[F[S, C]],
    EqFS: Eq[F[S, S]],
    EqFU: Eq[F[S, Unit]],
    ArbS: Arbitrary[S]
  ): RuleSet = {
    implicit def ArbFEA: Arbitrary[F[S, A]] = ArbF.synthesize[A]
    implicit def ArbFEB: Arbitrary[F[S, B]] = ArbF.synthesize[B]

    new RuleSet {
      def name: String = "monadState"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadState get idempotent" -> laws.monadStateGetIdempotent,
        "monadState set twice"      -> forAll(laws.monadStateSetTwice _),
        "monadState set get"        -> forAll(laws.monadStateSetGet _),
        "monadState get set"        -> laws.monadStateGetSet
      )
    }
  }
}

object MonadStateTests {
  def apply[F[_, _], S](implicit FS: MonadState[F, S], arbKFS: ArbitraryK[F[S, ?]], eqKFS: EqK[F[S, ?]]): MonadStateTests[F, S] =
    new MonadStateTests[F, S] {
      def arbitraryK: ArbitraryK[F[S, ?]] = arbKFS
      def eqK: EqK[F[S, ?]] = eqKFS
      def laws: MonadStateLaws[F, S] = MonadStateLaws[F, S]
    }
}
