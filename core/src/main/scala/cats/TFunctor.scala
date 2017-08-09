package cats

import simulacrum.typeclass

/**
 * This is an endofunctor in the category of endofunctors in `Skal`.
 *
 * `Skal` is the category of scala types. Functors in `Skal`
 * is encoded as `Functor`. The functors in `Skal` themselves forms
 * a category, let's denote it as `F[Skal]`.
 * In `F[Skal]`, functors of Skal, e.g. `Option[_]` and `Either[E, _]`, are objects,
 * while natural transformations, e.g. `Option ~> Either[E, ?]`, are arrows.
 * A endofunctor in `F[Skal]` maps one set of functors of `Skal` to another
 * set of functors of `Skal` while preserving the structures.
 *
 * For `TFunctor`, the domain is `F[_]`, the codomain is `T[F, _]`, both are
 * functors in `Skal`. The `TFunctor` provides a mapping from the arrows between
 * `F[_]` and `G[_]`, i.e. `F ~> G` to arrows between `T[F, _]` and `T[G, _]`,
 * i.e. `T[F, ?] ~> T[G, ?]`. The `lift` method makes this intention clear.
 *
 * In `cats.core`, examples of such `TFunctor`s are monad transformers such
 * as `OptionT`, `EitherT`
 *
 */
@typeclass trait TFunctor[T[_[_], _]] {
  def mapNT[F[_], G[_], A](h: T[F, A])(f: F ~> G): T[G, A]

  def liftNT[F[_], G[_]](f: F ~> G): T[F, ?] ~> T[G, ?] =
    λ[T[F, ?] ~> T[G, ?]](hf => mapNT(hf)(f))
}

