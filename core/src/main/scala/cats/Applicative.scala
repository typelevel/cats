package cats

import simulacrum._

/*
 * Applicative functor.
 * 
 * Must obey the following laws:
 *  - apply(fa)(pure(a => a)) = fa
 *  - apply(pure(a))(pure(f)) = pure(f(a))
 *  - apply(pure(a))(ff) = apply(ff)(pure(f => f(a)))
 *  - map(fa)(f) = apply(fa)(pure(f))
 */
@typeclass trait Applicative[F[_]] extends Apply[F] { self => 
  def pure[A](x: A): F[A]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))

  def compose[G[_]: Applicative]: Applicative[({type λ[α] = F[G[α]]})#λ] =
    new Applicative[({type λ[α] = F[G[α]]})#λ] {
      def pure[A](a: A): F[G[A]] = self.pure(Applicative[G].pure(a))

      def apply[A,B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
        self.apply(fa)(self.map(f)(gab => Applicative[G].apply(_)(gab)))
    }
}
