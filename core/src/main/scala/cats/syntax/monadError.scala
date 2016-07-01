package cats
package syntax

trait MonadErrorSyntax {
  implicit def catsSyntaxMonadError[F[_], E, A](fa: F[A])(implicit F: MonadError[F, E]): MonadErrorOps[F, E, A] =
    new MonadErrorOps(fa)
}

final class MonadErrorOps[F[_], E, A](fa: F[A])(implicit F: MonadError[F, E]) {
  def ensure(error: => E)(predicate: A => Boolean): F[A] =
    F.ensure(fa)(error)(predicate)
}
