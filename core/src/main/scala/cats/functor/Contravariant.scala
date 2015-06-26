package cats
package functor

import simulacrum._

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
@typeclass trait Contravariant[F[_]] extends Invariant[F] { self =>
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = contramap(fa)(fi)

  def compose[G[_]](implicit G: Contravariant[G]): Functor[Lambda[X => F[G[X]]]] = {
    val G0 = G
    new Contravariant.Composite[F, G] {
      def F: Contravariant[F] = self
      def G: Contravariant[G] = G0
    }
  }

  override def composeWithFunctor[G[_]](implicit G: Functor[G]): Contravariant[Lambda[X => F[G[X]]]] = {
    val G0 = G
    new Contravariant.CovariantComposite[F, G] {
      def F: Contravariant[F] = self
      def G: Functor[G] = G0
    }
  }
}

object Contravariant {
  trait Composite[F[_], G[_]] extends Functor[Lambda[X => F[G[X]]]] {
    def F: Contravariant[F]
    def G: Contravariant[G]
    def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.contramap(fga)(gb => G.contramap(gb)(f))
  }

  trait CovariantComposite[F[_], G[_]] extends Contravariant[Lambda[X => F[G[X]]]] {
    def F: Contravariant[F]
    def G: Functor[G]
    def contramap[A, B](fga: F[G[A]])(f: B => A): F[G[B]] = F.contramap(fga)(gb => G.map(gb)(f))
  }
}
