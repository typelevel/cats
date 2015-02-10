package cats
package functor

import simulacrum._

/**
 * Must obey the laws defined in [[laws.InvariantLaws]].
 */
@typeclass trait Invariant[F[_]] extends Any { self =>
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  /**
   * Compose 2 invariant Functors F and G to get a new Invariant Functor for F[G[_]].
   */
  def compose[G[_]: Invariant]: Invariant[Lambda[X => F[G[X]]]] = new Invariant.Composite[F, G] {
    def F: Invariant[F] = self
    def G: Invariant[G] = Invariant[G]
  }

  /**
   * Compose the Invariant Functor F with a normal (Covariant) Functor to get a new Invariant Functor for [F[G[_]].
   */
  def composeWithFunctor[G[_]: Functor]: Invariant[Lambda[X => F[G[X]]]] = new Invariant.CovariantComposite[F, G] {
    def F: Invariant[F] = self
    def G: Functor[G] = Functor[G]
  }

  /**
   * Compose the Invariant Functor F with a Contravariant Functor to get a new Invariant Functor for [F[G[_]]].
   */
  def composeWithContravariant[G[_]: Contravariant]: Invariant[Lambda[X => F[G[X]]]] = new Invariant.ContravariantComposite[F, G] {
    def F: Invariant[F] = self
    def G: Contravariant[G] = Contravariant[G]
  }
}

object Invariant {
  trait Composite[F[_], G[_]] extends Invariant[Lambda[X => F[G[X]]]] {
    def F: Invariant[F]
    def G: Invariant[G]

    override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
      F.imap(fga)(ga => G.imap(ga)(f)(g))(gb => G.imap(gb)(g)(f))
  }

  trait CovariantComposite[F[_], G[_]] extends Invariant[Lambda[X => F[G[X]]]] {
    def F: Invariant[F]
    def G: Functor[G]

    override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
      F.imap(fga)(ga => G.map(ga)(f))(gb => G.map(gb)(g))
  }

  trait ContravariantComposite[F[_], G[_]] extends Invariant[Lambda[X => F[G[X]]]] {
    def F: Invariant[F]
    def G: Contravariant[G]

    override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
      F.imap(fga)(ga => G.contramap(ga)(g))(gb => G.contramap(gb)(f))
  }
}
