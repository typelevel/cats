package cats
package syntax

trait MonadSyntax {
  implicit final def catsSyntaxMonad[F[_], A](fa: F[A]): MonadOps[F, A] = new MonadOps(fa)

  implicit final def catsSyntaxMonadIdOps[A](a: A): MonadIdOps[A] =
    new MonadIdOps[A](a)
}

final class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {
  def whileM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntil(fa)(p)
}

final class MonadIdOps[A](val a: A) extends AnyVal {

  /**
   * Iterative application of `f` while `p` holds.
   */
  def iterateWhileM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhileM(a)(f)(p)

  /**
   * Iterative application of `f` until `p` holds.
   */
  def iterateUntilM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntilM(a)(f)(p)
}
