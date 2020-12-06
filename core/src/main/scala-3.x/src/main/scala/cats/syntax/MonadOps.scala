package cats.syntax

import cats.{Monad, Alternative}

final class MonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def whileM[G[_]](using M: Monad[F], G: Alternative[G])(p: F[Boolean]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(using M: Monad[F])(p: F[Boolean]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](using M: Monad[F], G: Alternative[G])(p: F[Boolean]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(using M: Monad[F])(p: F[Boolean]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(using M: Monad[F])(p: A => Boolean): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(using M: Monad[F])(p: A => Boolean): F[A] = M.iterateUntil(fa)(p)
  def replicateM(using M: Monad[F])(n: Int): F[List[A]] = M.replicateM(n, fa)
  def replicateM_(using M: Monad[F])(n: Int): F[Unit] = M.replicateM_(n, fa)
}
