package cats

import simulacrum.typeclass

/**
 * [[Cartesian]] captures the idea of composing independent effectful values.
 * It is of particular interest when taken together with [[Functor]] - where [[Functor]]
 * captures the idea of applying a unary pure function to an effectful value,
 * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
 * independent effectful values.
 *
 * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
 * [[Cartesian]] and [[Functor]] to illustrate this.
 */
@typeclass trait Cartesian[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Cartesian extends CartesianArityFunctions with KernelCartesianInstances

/**
 * Cartesian instances for types that are housed in Kernel and therefore
 * can't have instances for Cats type classes in their companion objects.
 */
private[cats] sealed trait KernelCartesianInstances {
  implicit val catsInvariantSemigroup: Cartesian[Semigroup] = InvariantMonoidal.catsInvariantMonoidalSemigroup
  implicit val catsInvariantMonoid: Cartesian[Monoid] = InvariantMonoidal.catsInvariantMonoidalMonoid
  implicit val catsCartesianEq: Cartesian[Eq] = ContravariantCartesian.catsContravariantCartesianEq
}
