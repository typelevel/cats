package cats
package laws

import cats.data.EitherT

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait ApplicativeErrorLaws[F[_], E] extends ApplicativeLaws[F] {
  implicit def FE: ApplicativeError[F, E]
  implicit def F: Applicative[F] = FE.applicative

  def applicativeErrorHandleWith[A](e: E, f: E => F[A]): IsEq[F[A]] =
    FE.handleErrorWith(FE.raiseError[A](e))(f) <-> f(e)

  def applicativeErrorHandle[A](e: E, f: E => A): IsEq[F[A]] =
    FE.handleError(FE.raiseError[A](e))(f) <-> F.pure(f(e))

  def handleErrorWithPure[A](a: A, f: E => F[A]): IsEq[F[A]] =
    FE.handleErrorWith(F.pure(a))(f) <-> F.pure(a)

  def handleErrorPure[A](a: A, f: E => A): IsEq[F[A]] =
    FE.handleError(F.pure(a))(f) <-> F.pure(a)

  def raiseErrorAttempt(e: E): IsEq[F[Either[E, Unit]]] =
    FE.attempt(FE.raiseError[Unit](e)) <-> F.pure(Left(e))

  def pureAttempt[A](a: A): IsEq[F[Either[E, A]]] =
    FE.attempt(F.pure(a)) <-> F.pure(Right(a))

  def handleErrorWithConsistentWithRecoverWith[A](fa: F[A], f: E => F[A]): IsEq[F[A]] =
    FE.handleErrorWith(fa)(f) <-> FE.recoverWith(fa)(PartialFunction(f))

  def handleErrorConsistentWithRecover[A](fa: F[A], f: E => A): IsEq[F[A]] =
    FE.handleError(fa)(f) <-> FE.recover(fa)(PartialFunction(f))

  def recoverConsistentWithRecoverWith[A](fa: F[A], pf: PartialFunction[E, A]): IsEq[F[A]] =
    FE.recover(fa)(pf) <-> FE.recoverWith(fa)(pf andThen F.pure)

  def attemptConsistentWithAttemptT[A](fa: F[A]): IsEq[EitherT[F, E, A]] =
    EitherT(FE.attempt(fa)) <-> FE.attemptT(fa)
}

object ApplicativeErrorLaws {
  def apply[F[_], E](implicit ev: ApplicativeError[F, E]): ApplicativeErrorLaws[F, E] =
    new ApplicativeErrorLaws[F, E] { def FE: ApplicativeError[F, E] = ev }
}
