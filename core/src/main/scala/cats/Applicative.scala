package cats

import simulacrum._

/**
 * Applicative functor.
 * 
 * Must obey the following laws:
 *  - apply(fa)(pure(a => a)) = fa
 *  - apply(pure(a))(pure(f)) = pure(f(a))
 *  - apply(pure(a))(ff) = apply(ff)(pure(f => f(a)))
 *  - map(fa)(f) = apply(fa)(pure(f))
 */
@typeclass trait Applicative[F[_]] extends Apply[F] {
  def pure[A](x: A): F[A]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))
}
