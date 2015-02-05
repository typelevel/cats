package cats

import simulacrum._

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 *
 * Laws:
 *  - apply(apply(fa)(fab))(fbc) = apply(fa)(apply(fab)(map(fbc)(bc => ab => ab andThen bc)))
 */
trait Apply[F[_]] extends Functor[F] with ApplyArityFunctions[F] { self =>
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(f: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(f)(f => (b: B) => (a: A) => f(a, b))))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))

  def compose[G[_]](implicit GG: Apply[G]): Apply[({type λ[α] = F[G[α]]})#λ] =
    new CompositeApply[F,G] {
      implicit def F: Apply[F] = self
      implicit def G: Apply[G] = GG
    }
}

object Apply {
  def apply[F[_]](implicit ev: Apply[F]): Apply[F] = ev
}


trait CompositeApply[F[_],G[_]]
    extends Apply[λ[α => F[G[α]]]] with CompositeFunctor[F,G] {

  implicit def F: Apply[F]
  implicit def G: Apply[G]

  def apply[A,B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
    F.apply(fa)(F.map(f)(gab => G.apply(_)(gab)))
}
