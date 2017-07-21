package cats
package syntax

import cats.data.EitherT

trait ApplicativeErrorSyntax {
  implicit final def catsSyntaxApplicativeErrorId[E](e: E): ApplicativeErrorIdOps[E] =
    new ApplicativeErrorIdOps(e)

  implicit final def catsSyntaxApplicativeError[F[_], E, A](fa: F[A])(implicit F: ApplicativeError[F, E]): ApplicativeErrorOps[F, E, A] =
    new ApplicativeErrorOps[F, E, A](fa)
}

final class ApplicativeErrorIdOps[E](val e: E) extends AnyVal {
  def raiseError[F[_], A](implicit F: ApplicativeError[F, E]): F[A] =
    F.raiseError(e)
}

final class ApplicativeErrorOps[F[_], E, A](val fa: F[A]) extends AnyVal {
  def handleError(f: E => A)(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleError(fa)(f)

  def handleErrorWith(f: E => F[A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleErrorWith(fa)(f)

  def attempt(implicit F: ApplicativeError[F, E]): F[Either[E, A]] =
    F.attempt(fa)

  def attemptT(implicit F: ApplicativeError[F, E]): EitherT[F, E, A] =
    F.attemptT(fa)

  def recover(pf: PartialFunction[E, A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recover(fa)(pf)

  def recoverWith(pf: PartialFunction[E, F[A]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recoverWith(fa)(pf)

  def onError(pf: PartialFunction[E, F[Unit]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.onError(fa)(pf)
}
