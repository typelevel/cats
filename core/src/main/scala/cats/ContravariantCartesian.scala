package cats

import functor.Contravariant
import simulacrum.typeclass

/**
 * [[ContravariantCartesian]] is nothing more than something both contravariant
 * and Cartesian. It comes up enough to be useful, and composes well
 */
@typeclass trait ContravariantCartesian[F[_]] extends Cartesian[F] with Contravariant[F] { self =>
  override def composeFunctor[G[_]: Functor]: ContravariantCartesian[λ[α => F[G[α]]]] =
    new ComposedCartesian[F, G] {
      def F = self
      def G = Functor[G]
    }
}
