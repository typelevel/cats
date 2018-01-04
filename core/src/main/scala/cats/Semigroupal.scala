package cats

import simulacrum.typeclass

/**
 * [[Semigroupal]] captures the idea of composing independent effectful values.
 * It is of particular interest when taken together with [[Functor]] - where [[Functor]]
 * captures the idea of applying a unary pure function to an effectful value,
 * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
 * independent effectful values.
 *
 * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
 * [[Semigroupal]] and [[Functor]] to illustrate this.
 */
@typeclass trait Semigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Semigroupal extends SemigroupalArityFunctions
