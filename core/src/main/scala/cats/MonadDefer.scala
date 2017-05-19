package cats

import simulacrum.typeclass

/**
 * A [[Monad monad]] that allows for arbitrarily delaying the
 * evaluation of an operation, triggering its execution on each run.
 *
 * Instances of this type-class have the following properties:
 *
 *  - suspend any side-effects for later, until evaluated
 *  - suspension has `always` semantics, meaning that on each
 *    evaluation of `F[_]` the evaluation, along with any
 *    side-effects, get repeated
 *  - the `flatMap` operation is stack safe and can be
 *    used in recursive loops
 */
@typeclass trait MonadDefer[F[_]] extends Monad[F] {
  /**
   * Returns an `F[A]` that evaluates the provided by-name `fa`
   * parameter on each run. In essence it builds an `F[A]` factory.
   */
  def defer[A](fa: => F[A]): F[A]

  /** Lifts the given by-name value in the `F[_]` context. */
  def delay[A](a: => A): F[A] =
    defer(pure(a))
}
