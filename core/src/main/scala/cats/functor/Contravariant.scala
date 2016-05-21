package cats
package functor

import cats.data.{NestedContravariant, NestedContravariantCovariant}

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
@typeclass trait Contravariant[F[_]] extends Invariant[F] { self =>
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = contramap(fa)(fi)

  def nest[G[_]: Contravariant]: Functor[Lambda[A => F[G[A]]]] =
    new NestedContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }

  override def nestFunctor[G[_]: Functor]: Contravariant[Lambda[A => F[G[A]]]] =
    new NestedContravariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }
}
