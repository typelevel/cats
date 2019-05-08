package cats
package laws

import cats.data.EitherT

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait ApplicativeErrorLaws[F[_], E] extends ApplicativeLaws[F] {
  implicit override def F: ApplicativeError[F, E]

  def applicativeErrorHandleWith[A](e: E, f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(F.raiseError[A](e))(f) <-> f(e)

  def applicativeErrorHandle[A](e: E, f: E => A): IsEq[F[A]] =
    F.handleError(F.raiseError[A](e))(f) <-> F.pure(f(e))

  def handleErrorWithPure[A](a: A, f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(F.pure(a))(f) <-> F.pure(a)

  def handleErrorPure[A](a: A, f: E => A): IsEq[F[A]] =
    F.handleError(F.pure(a))(f) <-> F.pure(a)

  def raiseErrorAttempt(e: E): IsEq[F[Either[E, Unit]]] =
    F.attempt(F.raiseError[Unit](e)) <-> F.pure(Left(e))

  def pureAttempt[A](a: A): IsEq[F[Either[E, A]]] =
    F.attempt(F.pure(a)) <-> F.pure(Right(a))

  def handleErrorWithConsistentWithRecoverWith[A](fa: F[A], f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(fa)(f) <-> F.recoverWith(fa) { case x => f(x) }

  def handleErrorConsistentWithRecover[A](fa: F[A], f: E => A): IsEq[F[A]] =
    F.handleError(fa)(f) <-> F.recover(fa) { case x => f(x) }

  def recoverConsistentWithRecoverWith[A](fa: F[A], pf: PartialFunction[E, A]): IsEq[F[A]] =
    F.recover(fa)(pf) <-> F.recoverWith(fa)(pf.andThen(F.pure _))

  def attemptConsistentWithAttemptT[A](fa: F[A]): IsEq[EitherT[F, E, A]] =
    EitherT(F.attempt(fa)) <-> F.attemptT(fa)

  def attemptFromEitherConsistentWithPure[A](eab: Either[E, A]): IsEq[F[Either[E, A]]] =
    F.attempt(F.fromEither(eab)) <-> F.pure(eab)

  def onErrorPure[A](a: A, f: E => F[Unit]): IsEq[F[A]] =
    F.onError(F.pure(a)) { case x => f(x) } <-> F.pure(a)

  def onErrorRaise[A](fa: F[A], e: E, fb: F[Unit]): IsEq[F[A]] =
    F.onError(F.raiseError[A](e)) { case err => fb } <-> F.map2(fb, F.raiseError[A](e))((_, b) => b)
}

object ApplicativeErrorLaws {
  def apply[F[_], E](implicit ev: ApplicativeError[F, E]): ApplicativeErrorLaws[F, E] =
    new ApplicativeErrorLaws[F, E] { def F: ApplicativeError[F, E] = ev }
}
