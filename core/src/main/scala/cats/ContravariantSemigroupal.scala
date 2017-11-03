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
}
