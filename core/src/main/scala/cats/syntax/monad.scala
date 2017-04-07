package cats
package syntax

trait MonadSyntax {
  implicit def catsSyntaxMonad[F[_]: Monad, A](fa: F[A]): MonadOps[F, A] = new MonadOps(fa)
}

final class MonadOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
  def whileM[G[_]](p: F[Boolean])(implicit G: Alternative[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(p: F[Boolean]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](p: F[Boolean])(implicit G: Alternative[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(p: F[Boolean]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(p: A => Boolean): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(p: A => Boolean): F[A] = M.iterateUntil(fa)(p)
}
