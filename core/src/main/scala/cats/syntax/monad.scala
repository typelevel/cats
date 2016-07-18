package cats
package syntax

trait MonadSyntax {
  implicit def whileMSyntax[F[_]: Monad, A](fa: F[A]): WhileMOps[F, A] = new WhileMOps(fa)
  implicit def untilMSyntax[F[_]: Monad, A](fa: F[A]): UntilMOps[F, A] = new UntilMOps(fa)
  implicit def iterateWhileSyntax[F[_]: Monad, A](fa: F[A]): IterateWhileOps[F, A] = new IterateWhileOps(fa)
  implicit def iterateUntilSyntax[F[_]: Monad, A](fa: F[A]): IterateUntilOps[F, A] = new IterateUntilOps(fa)
}

final class WhileMOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
  def whileM[G[_]](p: F[Boolean])(implicit G: MonadCombine[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_[G[_]](p: F[Boolean])(implicit G: MonadCombine[G]): F[Unit] = M.whileM_(p)(fa)
}

final class UntilMOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
  def untilM[G[_]](p: F[Boolean])(implicit G: MonadCombine[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_[G[_]](p: F[Boolean])(implicit G: MonadCombine[G]): F[Unit] = M.untilM_(fa)(p)
}

final class IterateWhileOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
  def iterateWhile(p: A => Boolean): F[A] = M.iterateWhile(fa)(p)
}

final class IterateUntilOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
  def iterateUntil(p: A => Boolean): F[A] = M.iterateUntil(fa)(p)
}
