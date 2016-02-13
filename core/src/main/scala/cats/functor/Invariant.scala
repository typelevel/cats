package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
@typeclass trait Invariant[F[_]] { self =>
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  def compose[G[_]: Invariant]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariant[F, G] {
      val F = self
      val G = Invariant[G]
    }

  def composeFunctor[G[_]: Functor]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }

  def composeContravariant[G[_]: Contravariant]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariantContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }
}

object Invariant extends AlgebraInvariantInstances

/**
 * Invariant instances for types that are housed in Algebra and therefore
 * can't have instances for Cats type classes in their companion objects.
 */
private[functor] sealed trait AlgebraInvariantInstances {
  implicit val catsFunctorInvariantForSemigroup: Invariant[Semigroup] = InvariantMonoidal.invariantMonoidalSemigroup
  implicit val catsFunctorInvariantForMonoid: Invariant[Monoid] = InvariantMonoidal.invariantMonoidalMonoid
}
