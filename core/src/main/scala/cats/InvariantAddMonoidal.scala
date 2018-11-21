package cats

import simulacrum.typeclass
import cats.data.INothing

/**
 * An Invariant Additive Monoidal.
 * Adds the ability to create an `empty` value which must serve as the identity for `sum`.
 *
 * Must obey the laws defined in cats.laws.InvariantAddMonoidalLaws.
 */
@typeclass trait InvariantAddMonoidal[F[_]] extends InvariantAddSemigroupal[F] {
  def zero: F[INothing]
}
