package cats

import cats.kernel.CommutativeSemigroup
import simulacrum.typeclass

/**
 * Commutative Apply.
 *
 * Further than an Apply, which just allows composition of independent effectful functions,
 * in a Commutative Apply those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplyLaws.
 */
@typeclass trait CommutativeApply[F[_]] extends Apply[F]

object CommutativeApply {
  def commutativeSemigroupFor[F[_]: CommutativeApply, A: CommutativeSemigroup]: CommutativeSemigroup[F[A]] =
    new CommutativeSemigroup[F[A]] {
      override def combine(x: F[A], y: F[A]): F[A] =
        CommutativeApply[F]
          .map2(x, y)(CommutativeSemigroup[A].combine)
    }
}
