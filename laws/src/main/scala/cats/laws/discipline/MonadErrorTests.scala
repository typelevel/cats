package cats
package laws
package discipline

import cats.laws.discipline.arbitrary.partialFunctionArbitrary
import cats.laws.discipline.eq.unitEq
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadErrorTests[F[_], E] extends MonadTests[F] {
  def laws: MonadErrorLaws[F, E]

  implicit def arbitraryK: ArbitraryK[F]
  implicit def eqK: EqK[F]

  implicit def arbitraryE: Arbitrary[E]
  implicit def eqE: Eq[E]

  def monadError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet = {
    implicit def arbFT[T:Arbitrary]: Arbitrary[F[T]] = arbitraryK.synthesize
    implicit def eqFT[T:Eq]: Eq[F[T]] = eqK.synthesize

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
  def apply[F[_], E: Arbitrary: Eq](implicit FE: MonadError[F, E], ArbKF: ArbitraryK[F], EqKF: EqK[F]): MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] {
      def arbitraryE: Arbitrary[E] = implicitly[Arbitrary[E]]
      def arbitraryK: ArbitraryK[F] = ArbKF
      def eqE: Eq[E] = Eq[E]
      def eqK: EqK[F] = EqKF
      def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E]
    }
}
