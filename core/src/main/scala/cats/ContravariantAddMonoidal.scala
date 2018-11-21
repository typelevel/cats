package cats

import cats.data.INothing
import simulacrum.typeclass

/**
 * Contravariant version of an Additive Monoidal.
 *
 * Must obey the laws defined in cats.laws.ContravariantAddMonoidalLaws.
 */
@typeclass trait ContravariantAddMonoidal[F[_]] extends InvariantAddMonoidal[F] with ContravariantAddSemigroupal[F] {
  def lose[A](f: A => INothing): F[A] =
    contravariant.contramap(zero)(f)
}
