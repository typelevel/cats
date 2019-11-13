package cats
package laws
package discipline

import cats.data.EitherT
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

trait ApplicativeErrorTests[F[_], E] extends ApplicativeTests[F] {
  def laws: ApplicativeErrorLaws[F, E]

  def applicativeError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
                                                                             ArbFA: Arbitrary[F[A]],
                                                                             ArbFB: Arbitrary[F[B]],
                                                                             ArbFC: Arbitrary[F[C]],
                                                                             ArbFU: Arbitrary[F[Unit]],
                                                                             ArbFAtoB: Arbitrary[F[A => B]],
                                                                             ArbFBtoC: Arbitrary[F[B => C]],
                                                                             ArbE: Arbitrary[E],
                                                                             CogenA: Cogen[A],
                                                                             CogenB: Cogen[B],
                                                                             CogenC: Cogen[C],
                                                                             CogenE: Cogen[E],
                                                                             EqFA: Eq[F[A]],
                                                                             EqFB: Eq[F[B]],
                                                                             EqFC: Eq[F[C]],
                                                                             EqE: Eq[E],
                                                                             EqFEitherEU: Eq[F[Either[E, Unit]]],
                                                                             EqFEitherEA: Eq[F[Either[E, A]]],
                                                                             EqEitherTFEA: Eq[EitherT[F, E, A]],
                                                                             EqFABC: Eq[F[(A, B, C)]],
                                                                             iso: Isomorphisms[F]): RuleSet =
    new RuleSet {
      def name: String = "applicativeError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "applicativeError handleWith" -> forAll(laws.applicativeErrorHandleWith[A] _),
        "applicativeError handle" -> forAll(laws.applicativeErrorHandle[A] _),
        "applicativeError handleErrorWith pure" -> forAll(laws.handleErrorWithPure[A] _),
        "applicativeError handleError pure" -> forAll(laws.handleErrorPure[A] _),
        "applicativeError raiseError attempt" -> forAll(laws.raiseErrorAttempt _),
        "applicativeError pure attempt" -> forAll(laws.pureAttempt[A] _),
        "applicativeError handleErrorWith consistent with recoverWith" -> forAll(
          laws.handleErrorWithConsistentWithRecoverWith[A] _
        ),
        "applicativeError handleError consistent with recover" -> forAll(laws.handleErrorConsistentWithRecover[A] _),
        "applicativeError recover consistent with recoverWith" -> forAll(laws.recoverConsistentWithRecoverWith[A] _),
        "applicativeError attempt consistent with attemptT" -> forAll(laws.attemptConsistentWithAttemptT[A] _),
        "applicativeError attempt fromEither consistent with pure" -> forAll(
          laws.attemptFromEitherConsistentWithPure[A] _
        ),
        "applicativeError onError pure" -> forAll(laws.onErrorPure[A] _),
        "applicativeError onError raise" -> forAll(laws.onErrorRaise[A] _),
        "applicativeError adaptError pure" -> forAll(laws.adaptErrorPure[A] _),
        "applicativeError adaptError raise" -> forAll(laws.adaptErrorRaise[A] _)
      )
    }
}

object ApplicativeErrorTests {
  def apply[F[_], E](implicit FE: ApplicativeError[F, E]): ApplicativeErrorTests[F, E] =
    new ApplicativeErrorTests[F, E] {
      def laws: ApplicativeErrorLaws[F, E] = ApplicativeErrorLaws[F, E]
    }
}
