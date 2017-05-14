package cats
package syntax

trait MonadErrorSyntax {
  implicit final def catsSyntaxMonadError[F[_], E, A](fa: F[A]): MonadErrorOps[F, E, A] =
    new MonadErrorOps(fa)
}

final class MonadErrorOps[F[_], E, A](val fa: F[A]) extends AnyVal {
  def ensure(error: => E)(predicate: A => Boolean)(implicit F: MonadError[F, E]): F[A] =
    F.ensure(fa)(error)(predicate)
}
