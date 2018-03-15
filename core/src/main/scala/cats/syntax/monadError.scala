package cats
package syntax

trait MonadErrorSyntax {
  implicit final def catsSyntaxMonadError[F[_], E, A](fa: F[A])(implicit F: MonadError[F, E]): MonadErrorOps[F, E, A] =
    new MonadErrorOps(fa)

  implicit final def catsSyntaxMonadErrorRethrow[F[_], E, A](fea: F[Either[E, A]])(implicit F: MonadError[F, E]): MonadErrorRethrowOps[F, E, A] =
    new MonadErrorRethrowOps(fea)
}

final class MonadErrorOps[F[_], E, A](val fa: F[A]) extends AnyVal {
  def ensure(error: => E)(predicate: A => Boolean)(implicit F: MonadError[F, E]): F[A] =
    F.ensure(fa)(error)(predicate)

  def ensureOr(error: A => E)(predicate: A => Boolean)(implicit F: MonadError[F, E]): F[A] =
    F.ensureOr(fa)(error)(predicate)

  def reject(pf: PartialFunction[A, E])(implicit F: MonadError[F, E]): F[A] =
    F.flatMap(fa) { a =>
      pf.andThen(F.raiseError[A]).applyOrElse(a, (_: A) => fa)
    }

  def adaptError(pf: PartialFunction[E, E])(implicit F: MonadError[F, E]): F[A] =
    F.adaptError(fa)(pf)
}

final class MonadErrorRethrowOps[F[_], E, A](val fea: F[Either[E, A]]) extends AnyVal {
  def rethrow(implicit F: MonadError[F, E]): F[A] = F.rethrow(fea)
}
