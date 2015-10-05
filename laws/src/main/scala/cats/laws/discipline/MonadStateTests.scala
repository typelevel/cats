package cats
package laws
package discipline

import eq.unitEq
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadStateTests[F[_], S] extends MonadTests[F] {
  def laws: MonadStateLaws[F, S]

  implicit def arbitraryK: ArbitraryK[F]
  implicit def eqK: EqK[F]

  def monadState[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    EqS: Eq[S],
    ArbS: Arbitrary[S]
  ): RuleSet = {
    implicit def ArbFEA: Arbitrary[F[A]] = arbitraryK.synthesize[A]
    implicit def ArbFEB: Arbitrary[F[B]] = arbitraryK.synthesize[B]
    implicit def EqFA: Eq[F[A]] = eqK.synthesize[A]
    implicit def EqFB: Eq[F[B]] = eqK.synthesize[B]
    implicit def EqFC: Eq[F[C]] = eqK.synthesize[C]
    implicit def EqFS: Eq[F[S]] = eqK.synthesize[S]
    implicit def EqFUnit: Eq[F[Unit]] = eqK.synthesize[Unit]

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
  def apply[F[_], S](implicit FS: MonadState[F, S], arbKFS: ArbitraryK[F], eqKFS: EqK[F]): MonadStateTests[F, S] =
    new MonadStateTests[F, S] {
      def arbitraryK: ArbitraryK[F] = arbKFS
      def eqK: EqK[F] = eqKFS
      def laws: MonadStateLaws[F, S] = MonadStateLaws[F, S]
    }
}
