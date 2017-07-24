package cats
package syntax

import cats.data.EitherT

trait ApplicativeErrorSyntax {
  implicit final def catsSyntaxApplicativeErrorId[E](e: E): ApplicativeErrorIdOps[E] =
    new ApplicativeErrorIdOps(e)

  implicit final def catsSyntaxApplicativeError[F[_], A](fa: F[A]): ApplicativeErrorOps[F, A] =
    new ApplicativeErrorOps[F, A](fa)
}

final class ApplicativeErrorIdOps[E](val e: E) extends AnyVal {
  def raiseError[F[_], A](implicit F: ApplicativeError[F, E]): F[A] =
    F.raiseError(e)
}

final class ApplicativeErrorOps[F[_], A](val fa: F[A]) extends AnyVal {
  def handleError[E](f: E => A)(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleError(fa)(f)

  def handleErrorWith[E](f: E => F[A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleErrorWith(fa)(f)

  def attempt[E](implicit F: ApplicativeError[F, E]): F[Either[E, A]] =
    F.attempt(fa)

  def attemptT[E](implicit F: ApplicativeError[F, E]): EitherT[F, E, A] =
    F.attemptT(fa)

  def recover[E](pf: PartialFunction[E, A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recover(fa)(pf)

  def recoverWith[E](pf: PartialFunction[E, F[A]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recoverWith(fa)(pf)

  def onError(pf: PartialFunction[E, F[Unit]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.onError(fa)(pf)
}
