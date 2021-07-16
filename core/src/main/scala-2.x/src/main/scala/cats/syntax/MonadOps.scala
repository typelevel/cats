package cats.syntax

import cats.{Alternative, Monad}

final class MonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def whileM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntil(fa)(p)
  def replicateM(n: Int)(implicit M: Monad[F]): F[List[A]] = M.replicateM(n, fa)
  def replicateM_(n: Int)(implicit M: Monad[F]): F[Unit] = M.replicateM_(n, fa)
}
