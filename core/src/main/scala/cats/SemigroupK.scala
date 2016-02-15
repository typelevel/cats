package cats

import simulacrum.{op, typeclass}

/**
 * SemigroupK is a universal semigroup which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] has a
 * structure that can be combined for any particular type. Thus,
 * SemigroupK is like a Semigroup for kinds (i.e. parameterized
 * types).
 *
 * A SemigroupK[F] can produce a Semigroup[F[A]] for any type A.
 *
 * Here's how to distinguish Semigroup and SemigroupK:
 *
 *  - Semigroup[A] allows two A values to be combined.
 *
 *  - SemigroupK[F] allows two F[A] values to be combined, for any A.
 *    The combination operation just depends on the structure of F,
 *    but not the structure of A.
 */
@typeclass trait SemigroupK[F[_]] { self =>

  /**
   * Combine two F[A] values.
   */
  @op("<+>", alias=true)
  def combineK[A](x: F[A], y: F[A]): F[A]

  /**
   * Compose this SemigroupK with an arbitrary type constructor
   */
  def composeK[G[_]]: SemigroupK[λ[α => F[G[α]]]] =
    new CompositeSemigroupK[F, G] {
      implicit def F: SemigroupK[F] = self
    }

  /**
   * Given a type A, create a concrete Semigroup[F[A]].
   */
  def algebra[A]: Semigroup[F[A]] =
    new Semigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] = self.combineK(x, y)
    }
}

trait CompositeSemigroupK[F[_],G[_]]
  extends SemigroupK[λ[α => F[G[α]]]] {

  implicit def F: SemigroupK[F]

  def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.combineK(x, y)
}
