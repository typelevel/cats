package cats

import cats.kernel.CommutativeMonoid
import simulacrum.typeclass

/**
 * Commutative Applicative.
 *
 * Further than an Applicative, which just allows composition of independent effectful functions,
 * in a Commutative Applicative those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplicativeLaws.
 */
@typeclass trait CommutativeApplicative[F[_]] extends Applicative[F] with CommutativeApply[F]

object CommutativeApplicative {
  def commutativeMonoidFor[F[_]: CommutativeApplicative, A: CommutativeMonoid]: CommutativeMonoid[F[A]] =
    new CommutativeMonoid[F[A]] {
      override def empty: F[A] =
        CommutativeApplicative[F]
          .pure(CommutativeMonoid[A].empty)

      override def combine(x: F[A], y: F[A]): F[A] =
        CommutativeApplicative[F]
          .map2(x, y)(CommutativeMonoid[A].combine)
    }
}
