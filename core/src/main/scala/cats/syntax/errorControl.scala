package cats
package syntax

import cats.data.EitherT


trait ErrorControlSyntax {
  implicit final def catsSyntaxErrorControlF[F[_], E, A]
  (fa: F[A])(implicit F: MonadError[F, E]): ErrorControlFOps[F, E, A] =
    new ErrorControlFOps[F, E, A](fa)

  implicit final def catsSyntaxErrorControlG[G[_], A]
  (ga: G[A])(implicit G: Applicative[G]): ErrorControlGOps[G, A] =
    new ErrorControlGOps[G, A](ga)

  implicit final def catsSyntaxErrorControlEither[G[_], E, A]
  (gea: G[Either[E, A]])(implicit G: Applicative[G]): ErrorControlEitherOps[G, E, A] =
    new ErrorControlEitherOps[G, E, A](gea)

}


final class ErrorControlFOps[F[_], E, A](val fa: F[A]) extends AnyVal {

  def controlError[G[_]](f: E => G[A])(implicit E: ErrorControl[F, G, E]): G[A] =
    E.controlError(fa)(f)

  def trial[G[_]](implicit E: ErrorControl[F, G, E]): G[Either[E, A]] =
    E.trial(fa)

  def trialT[G[_]](implicit E: ErrorControl[F, G, E]): EitherT[G, E, A] =
    E.trialT(fa)

  def intercept[G[_]](f: E => A)(implicit E: ErrorControl[F, G, E]): G[A] =
    E.intercept(fa)(f)
}

final class ErrorControlGOps[G[_], A](val ga: G[A]) extends AnyVal {
  def assure[F[_]]: AssurePartiallyApplied[F, G, A] = new AssurePartiallyApplied[F, G, A](ga)

  def assureOr[F[_]]: AssureOrPartiallyApplied[F, G, A] = new AssureOrPartiallyApplied[F, G, A](ga)

  def accept[F[_]]: AcceptPartiallyApplied[F, G, A] = new AcceptPartiallyApplied[F, G, A](ga)
}

final class ErrorControlEitherOps[G[_], E, A](val gea: G[Either[E, A]]) extends AnyVal {
  def absolve[F[_]](implicit E: ErrorControl[F, G, E]): F[A] =
    E.absolve(gea)
}

private[syntax] final class AssurePartiallyApplied[F[_], G[_], A](val ga: G[A]) extends AnyVal {
  def apply[E](error: => E)
              (predicate: A => Boolean)
              (implicit E: ErrorControl[F, G, E]): F[A] =
    E.assure(ga)(error)(predicate)
}

private[syntax] final class AssureOrPartiallyApplied[F[_], G[_], A](val ga: G[A]) extends AnyVal {
  def apply[E](error: A => E)
              (predicate: A => Boolean)
              (implicit E: ErrorControl[F, G, E]): F[A] =
    E.assureOr(ga)(error)(predicate)
}

private[syntax] final class AcceptPartiallyApplied[F[_], G[_], A](val ga: G[A]) extends AnyVal {
  def apply[E](implicit E: ErrorControl[F, G, E]): F[A] =
    E.accept(ga)
}
