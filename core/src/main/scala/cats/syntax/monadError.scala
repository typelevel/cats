package cats
package syntax

trait MonadErrorSyntax {
  implicit final def catsSyntaxMonadError[F[_], E, A](fa: F[A])(implicit F: MonadError[F, E]): MonadErrorOps[F, E, A] =
    new MonadErrorOps(fa)

  implicit final def catsSyntaxMonadErrorRethrow[F[_], E, A](
    fea: F[Either[E, A]]
  )(implicit F: MonadError[F, _ >: E]): MonadErrorRethrowOps[F, E, A] =
    new MonadErrorRethrowOps(fea)
}

final class MonadErrorOps[F[_], E, A](private val fa: F[A]) extends AnyVal {
  def ensure(error: => E)(predicate: A => Boolean)(implicit F: MonadError[F, E]): F[A] =
    F.ensure(fa)(error)(predicate)

  def ensureOr(error: A => E)(predicate: A => Boolean)(implicit F: MonadError[F, E]): F[A] =
    F.ensureOr(fa)(error)(predicate)

  /**
   * Turns a successful value into the error returned by a given partial function if it is
   * in the partial function's domain.
   */
  def reject(pf: PartialFunction[A, E])(implicit F: MonadError[F, E]): F[A] =
    F.flatMap(fa) { a =>
      pf.andThen(F.raiseError[A] _).applyOrElse(a, F.pure)
    }

  def adaptError(pf: PartialFunction[E, E])(implicit F: MonadError[F, E]): F[A] =
    F.adaptError(fa)(pf)

  /**
   * Returns a new value that transforms the result of the source,
   * given the `recover` or `bind` functions, which get executed depending
   * on whether the result is successful or if it ends in error.
   *
   * This is an optimization on usage of [[ApplicativeError.attempt]] and [[FlatMap.flatMap]],
   * this equivalence being available:
   *
   * {{{
   *   fa.redeemWith(fe, fs) <-> fa.attempt.flatMap(_.fold(fe, fs))
   * }}}
   *
   * Usage of `redeemWith` subsumes [[ApplicativeError.handleErrorWith]] because:
   *
   * {{{
   *   fa.redeemWith(fe, F.pure) <-> fa.handleErrorWith(fe)
   * }}}
   *
   * Usage of `redeemWith` also subsumes [[FlatMap.flatMap]] because:
   *
   * {{{
   *   fa.redeemWith(F.raiseError, fs) <-> fa.flatMap(fs)
   * }}}
   *
   * @see [[ApplicativeErrorOps.redeem]], [[ApplicativeError.attempt]] and [[ApplicativeError.handleErrorWith]]
   *
   * @param recover is the function that gets called to recover the source
   *        in case of error
   * @param bind is the function that gets to transform the source
   *        in case of success
   */
  def redeemWith[B](recover: E => F[B], bind: A => F[B])(implicit F: MonadError[F, E]): F[B] =
    F.flatMap(F.attempt(fa))(_.fold(recover, bind))
}

final class MonadErrorRethrowOps[F[_], E, A](private val fea: F[Either[E, A]]) extends AnyVal {
  def rethrow(implicit F: MonadError[F, _ >: E]): F[A] =
    F.flatMap(fea)(_.fold(F.raiseError, F.pure)) // dup from the type class impl, due to https://github.com/scala/bug/issues/11562. Once fixed should be able to replace with `F.rethrow(fea)`
}
