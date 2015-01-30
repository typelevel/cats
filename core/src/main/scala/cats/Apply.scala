package cats

import simulacrum._

/*
 * Weaker version of Applicative[F]; has apply but not pure.
 * 
 * Laws:
 *  - apply(apply(fa)(fab))(fbc) = apply(fa)(apply(fab)(map(fbc)(bc => ab => ab andThen bc)))
 */
@typeclass trait Apply[F[_]] extends Functor[F] { self =>
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(f: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(f)(ff => (b: B) => (a: A) => ff(a, b))))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))

  def compose[G[_]: Apply]: Apply[({type λ[α] = F[G[α]]})#λ] =
    new Apply[({type λ[α] = F[G[α]]})#λ] {
      def map[A,B](fa: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fa)(Functor[G].lift(f))

      def apply[A,B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
        self.apply(fa)(self.map(f)(gab => Apply[G].apply(_)(gab)))
    }
}
