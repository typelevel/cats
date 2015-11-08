package cats
package laws
package discipline

import cats.data.{ Xor, XorT }
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.unitEq
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadErrorTests[F[_], E] extends MonadTests[F] {
  def laws: MonadErrorLaws[F, E]

  def monadError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbE: Arbitrary[E],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqE: Eq[E],
    EqFXorEU: Eq[F[E Xor Unit]],
    EqFXorEA: Eq[F[E Xor A]],
    EqXorTFEA: Eq[XorT[F, E, A]]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monadError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadError left zero" -> forAll(laws.monadErrorLeftZero[A, B] _),
        "monadError handleWith" -> forAll(laws.monadErrorHandleWith[A] _),
        "monadError handle" -> forAll(laws.monadErrorHandleWith[A] _),
        "monadError handleErrorWith pure" -> forAll(laws.handleErrorWithPure[A] _),
        "monadError handleError pure" -> forAll(laws.handleErrorPure[A] _),
        "monadError raiseError attempt" -> forAll(laws.raiseErrorAttempt _),
        "monadError pure attempt" -> forAll(laws.pureAttempt[A] _),
        "monadError handleErrorWith consistent with recoverWith" -> forAll(laws.handleErrorWithConsistentWithRecoverWith[A] _),
        "monadError handleError consistent with recover" -> forAll(laws.handleErrorConsistentWithRecover[A] _),
        "monadError recover consistent with recoverWith" -> forAll(laws.recoverConsistentWithRecoverWith[A] _),
        "monadError attempt consistent with attemptT" -> forAll(laws.attemptConsistentWithAttemptT[A] _)
      )
    }
  }
}

object MonadErrorTests {
  def apply[F[_], E](implicit FE: MonadError[F, E]): MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] {
      def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E]
    }
}
