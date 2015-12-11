package cats

import simulacrum.typeclass

/**
 * Monoidal allows us to express uncurried function application within a context,
 * whatever the context variance is.
 *
 * It is worth noting that the couple Monoidal and [[Functor]] is interdefinable with [[Apply]].
 */
@typeclass trait Monoidal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Monoidal extends MonoidalArityFunctions