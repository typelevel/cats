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

  @inline final def *>[A, B](fa: F[A])(fb: F[B])(implicit F: Functor[F]): F[B] =
    F.map(product(fa, fb)) { case (_, b) => b }

  @inline final def <*[A, B](fa: F[A])(fb: F[B])(implicit F: Functor[F]): F[A] =
    F.map(product(fa, fb)) { case (a, _) => a }
}

object Cartesian extends CartesianArityFunctions
