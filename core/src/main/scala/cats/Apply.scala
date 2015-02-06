package cats

import simulacrum._

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 *
 * Laws:
 *  - apply(apply(fa)(fab))(fbc) = apply(fa)(apply(fab)(map(fbc)(bc => ab => ab andThen bc)))
 */
@typeclass trait Apply[F[_]] extends Functor[F] { self =>
  /** Given a value and a function in the Apply context, applies the function to the value. */
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  /** apply2 is a binary version of apply, itself defined in terms of apply */
  def apply2[A, B, Z](fa: F[A], fb: F[B])(f: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(f)(ff => (b: B) => (a: A) => ff(a, b))))

  /**
   * Applies the pure (binary) function f to the effectful values fa and fb.
   *
   * map2 can be seen as a binary version of [[cats.Functor]]#map.
   */
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))

  /**
   * Two sequentially dependent Applys can be composed.
   *
   * The composition of Applys `F` and `G`, `F[G[x]]`, is also an Apply.
   *
   * Apply[Option].compose[List].map2(Some(List(1, 2)), Some(List(3)))(_ + _)
   * = Some(List(4, 5))
   */
  def compose[G[_]](implicit GG: Apply[G]): Apply[({type λ[α] = F[G[α]]})#λ] =
    new CompositeApply[F,G] {
      implicit def F: Apply[F] = self
      implicit def G: Apply[G] = GG
    }
}

trait CompositeApply[F[_],G[_]]
    extends Apply[λ[α => F[G[α]]]] with CompositeFunctor[F,G] {

  implicit def F: Apply[F]
  implicit def G: Apply[G]

  def apply[A,B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
    F.apply(fa)(F.map(f)(gab => G.apply(_)(gab)))
}
