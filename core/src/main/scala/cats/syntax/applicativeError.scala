package cats
package syntax

import cats.data.{Xor, XorT}

trait ApplicativeErrorSyntax {
  implicit def catsSyntaxApplicativeErrorId[E](e: E): ApplicativeErrorIdOps[E] =
    new ApplicativeErrorIdOps(e)

  implicit def catsSyntaxApplicativeError[F[_], E, A](fa: F[A])(implicit F: ApplicativeError[F, E]): ApplicativeErrorOps[F, E, A] =
    new ApplicativeErrorOps[F, E, A](fa)

}

final class ApplicativeErrorIdOps[E](e: E) {
  def raiseError[F[_], A](implicit F: ApplicativeError[F, E]): F[A] =
    F.raiseError(e)
}

final class ApplicativeErrorOps[F[_], E, A](fa: F[A])(implicit F: ApplicativeError[F, E]) {
  def handleError(f: E => A): F[A] =
    F.handleError(fa)(f)

  def handleErrorWith(f: E => F[A]): F[A] =
    F.handleErrorWith(fa)(f)

  def attempt: F[Xor[E, A]] =
    F.attempt(fa)

  def attemptT: XorT[F, E, A] =
    F.attemptT(fa)

  def recover(pf: PartialFunction[E, A]): F[A] =
    F.recover(fa)(pf)

  def recoverWith(pf: PartialFunction[E, F[A]]): F[A] =
    F.recoverWith(fa)(pf)
}
