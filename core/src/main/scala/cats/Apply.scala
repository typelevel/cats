package cats

import simulacrum._

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 * 
 * Laws:
 *  - apply(apply(fa)(fab))(fbc) = apply(fa)(apply(fab)(map(fbc)(bc => ab => ab andThen bc)))
 */
@typeclass trait Apply[F[_]] extends Functor[F] {
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))
}
