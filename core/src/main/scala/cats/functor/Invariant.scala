package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
@typeclass trait Invariant[F[_]] { self =>
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

object Invariant extends AlgebraInvariantInstances

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
