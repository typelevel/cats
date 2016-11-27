package cats

import simulacrum.typeclass

/**
 * Monad.
 *
 * Allows composition of dependent effectful functions.
 *
 * See: [[http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf Monads for functional programming]]
 *
 * Must obey the laws defined in cats.laws.MonadLaws.
 */
@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F] { self =>
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  def composeTraverseMonad[G[_]](implicit tg: Traverse[G], mg: Monad[G]): Monad[λ[α => F[G[α]]]] =
    new ComposedTraverseMonad[F, G] {
      def F = self
      def G = mg
      def TG = tg
    }
}
