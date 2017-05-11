package cats

import cats.functor.Invariant
import simulacrum.typeclass

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
@typeclass trait InvariantMonoidal[F[_]] extends Invariant[F] with Cartesian[F] {
  def pure[A](a: A): F[A]
}
