package cats
package syntax

trait MonadSyntax {
  implicit final def catsSyntaxMonad[F[_], A](fa: F[A]): MonadOps[F, A] = new MonadOps(fa)

  implicit final def catsSyntaxMonadIdOps[A](a: A): MonadIdOps[A] =
    new MonadIdOps[A](a)
}

final class MonadIdOps[A](private val a: A) extends AnyVal {

  /**
   * Iterative application of `f` while `p` holds.
   */
  def iterateWhileM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhileM(a)(f)(p)

  /**
   * Iterative application of `f` until `p` holds.
   */
  def iterateUntilM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntilM(a)(f)(p)
}
