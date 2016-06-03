package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
@typeclass trait Contravariant[F[_]] extends Invariant[F] { self =>
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = contramap(fa)(fi)

  def compose[G[_]: Contravariant]: Functor[Lambda[A => F[G[A]]]] =
    new ComposedContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }

  override def composeFunctor[G[_]: Functor]: Contravariant[Lambda[A => F[G[A]]]] =
    new ComposedContravariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }
}
