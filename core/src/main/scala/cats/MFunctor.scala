package cats

import simulacrum.typeclass

/**
 * Functor in the category of functors.
 *
 * If the ordinary `Functor` lifts a function `A => B` to `F[A] => F[B]`, then
 * this lifts a natural transformation `M ~> N` to `F[M] => F[N]`, where
 * `~>` represents the type `M[A] => N[A]`, for all `A`.
 *
 * All monad transformers are an instance of this, with the underlying monad
 * being the subject of the lifted transformation.
 *
 */

@typeclass trait MFunctor[F[_[_]]] {

 /**
  * Sometimes we know more about the underlying `F`, so the transformation
  * can be constrained to reflect this knowledge.
  *
  * In case of monad transformers, for instance, we always know that `F`
  * is a monad.
  */
  type C[M[_]]

  /**
   * Analogous to `lift` of `Functor`
   */
  def hoist[M[_]: C, N[_]: C](m: M ~> N): F[M] => F[N]

}
