package cats

import simulacrum.typeclass

/**
 * [[InvariantAddSemigroupal]] captures the idea of composing independent effectful values into a sum type.
 * It can be seen as an [[InvariantSemigroupal]] that produces a sum type instead of a product type, hence the name.
 *
 * Must obey the laws defined in cats.laws.InvariantAddMonoidalLaws.
 */
@typeclass trait InvariantAddSemigroupal[F[_]] extends Serializable {
  def invariant: Invariant[F]
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}
