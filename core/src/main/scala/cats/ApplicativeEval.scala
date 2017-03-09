package cats

import simulacrum.typeclass
import cats.Eval.always

/**
 * A type class that allows lifting any value into the applicative
 * context, with its evaluation being controlled by [[Eval]] and
 * supporting optional laziness.
 */
@typeclass trait ApplicativeEval[F[_]] extends Applicative[F] {
  /**
   * Lifts any value into the `F[_]` applicative context, where the
   * evaluation is controlled by [[Eval]] and can be optionally lazy.
   */
  def eval[A](a: Eval[A]): F[A]

  /**
   * Lifts the given by-name value in the `F[_]` context, with
   * optional laziness.
   *
   * Alias for `eval(always(a))`.
   */
  final def delay[A](a: => A): F[A] = eval(always(a))
}
