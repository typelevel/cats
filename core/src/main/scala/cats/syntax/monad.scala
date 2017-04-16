package cats
package syntax

trait MonadSyntax {
  implicit final def catsSyntaxMonad[F[_], A](fa: F[A]): MonadOps[F, A] = new MonadOps(fa)
}

final class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {
  def whileM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntil(fa)(p)
}
