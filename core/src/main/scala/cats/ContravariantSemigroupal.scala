package cats

import simulacrum.typeclass

/**
 * [[ContravariantSemigroupal]] is nothing more than something both contravariant
 * and Semigroupal. It comes up enough to be useful, and composes well
 */
@typeclass trait ContravariantSemigroupal[F[_]] extends Semigroupal[F] with Contravariant[F] { self =>
  override def composeFunctor[G[_]: Functor]: ContravariantSemigroupal[λ[α => F[G[α]]]] =
    new ComposedSemigroupal[F, G] {
      def F = self
      def G = Functor[G]
    }

  def contramap2[A, B, C](fb: F[B], fc: F[C])(f: A => (B, C)): F[A] =
    contramap(product(fb, fc))(f)
}
