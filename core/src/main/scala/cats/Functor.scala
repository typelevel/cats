package cats

import simulacrum._

/**
 * Functor.
 *
 * Must obey the following laws:
 *  - map(fa)(identity) = fa
 *  - map(map(fa)(f1))(f2) = map(fa)(f2 compose f1)
 */
@typeclass trait Functor[F[_]] extends functor.Invariant[F] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def fmap[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
  def imap[A, B](fa: F[A])(f: A <=> B): F[B] = map(fa)(f)
}
