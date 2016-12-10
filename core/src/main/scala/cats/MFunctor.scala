package cats

/**
 * A type class which abstracts over the ability to lift a natural transformation
 * (monad morphism) `M ~> N` into a MonadTransformer. In other words, a functor
 * in the category of monads.
 */

trait MFunctor[MT[_[_]]] {

  /**
   * Analogous to fmap
   */
  def hoist[M[_], N[_]](m: M ~> N): MT[M] => MT[N]

}
