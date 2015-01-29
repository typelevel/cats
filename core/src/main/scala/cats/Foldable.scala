package cats

import algebra.Monoid
import cats.arrow.MonoidK
import simulacrum._

@typeclass trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], b: B)(f: (A, B) => B): B

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B =
    foldLeft(fa, Monoid[B].empty) { (b, a) =>
      Monoid[B].combine(b, f(a))
    }

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(x => x)

  def traverse_[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[Unit] =
    foldLeft(fa, Applicative[G].pure(())) { (acc, a) =>
      Applicative[G].map2(acc, f(a)) { (_, _) => () }
    }

  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  def psum[G[_]: MonoidK, A](fga: F[G[A]]): G[A] =
    foldLeft(fga, MonoidK[G].empty[A])(MonoidK[G].combine)
}
