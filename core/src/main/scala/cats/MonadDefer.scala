package cats

import simulacrum.typeclass
import cats.Eval.always

/**
 * A [[Monad monad]] that allows for arbitrarily delaying the
 * evaluation of an operation, triggering its execution on each run.
 *
 * @see [[ApplicativeEval]] for capturing effects in an `F[_]`
 *     applicative context, but without the repeating side-effects
 *     requirement.
 */
@typeclass trait MonadDefer[F[_]] extends Monad[F] with ApplicativeEval[F] {
  /**
   * Returns an `F[A]` that evaluates the provided by-name `fa`
   * parameter on each run. In essence it builds an `F[A]` factory.
   */
  def defer[A](fa: => F[A]): F[A] =
    flatten(eval(always(fa)))
}
