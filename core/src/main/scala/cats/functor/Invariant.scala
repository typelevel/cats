package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
@typeclass trait Invariant[F[_]] { self =>
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  /**
   * Compose 2 invariant Functors F and G to get a new Invariant Functor for F[G[_]].
   */
  def compose[G[_]: Invariant](implicit GG: Invariant[G]): Invariant[Lambda[X => F[G[X]]]] = new Invariant.Composite[F, G] {
    def F: Invariant[F] = self
    def G: Invariant[G] = GG
  }

  /**
   * Compose the Invariant Functor F with a normal (Covariant) Functor to get a new Invariant Functor for [F[G[_]].
   */
  def composeWithFunctor[G[_]](implicit GG: Functor[G]): Invariant[Lambda[X => F[G[X]]]] = new Invariant.CovariantComposite[F, G] {
    def F: Invariant[F] = self
    def G: Functor[G] = GG
  }

  /**
   * Compose the Invariant Functor F with a Contravariant Functor to get a new Invariant Functor for [F[G[_]]].
   */
  def composeWithContravariant[G[_]](implicit GG: Contravariant[G]): Invariant[Lambda[X => F[G[X]]]] = new Invariant.ContravariantComposite[F, G] {
    def F: Invariant[F] = self
    def G: Contravariant[G] = GG
  }
}

object Invariant extends AlgebraInvariantInstances {
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

/**
 * Invariant instances for types that are housed in Algebra and therefore
 * can't have instances for Cats type classes in their companion objects.
 */
private[functor] sealed trait AlgebraInvariantInstances {

  implicit val invariantSemigroup: Invariant[Semigroup] = new Invariant[Semigroup] {
    def imap[A, B](fa: Semigroup[A])(f: A => B)(g: B => A): Semigroup[B] = new Semigroup[B] {

      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }
  }

  implicit val invariantMonoid: Invariant[Monoid] = new Invariant[Monoid] {
    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      val empty = f(fa.empty)

      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }
  }
}
