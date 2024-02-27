/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package syntax

trait MonadErrorSyntax {
  implicit final def catsSyntaxMonadError[F[_], E, A](fa: F[A])(implicit F: MonadError[F, E]): MonadErrorOps[F, E, A] =
    new MonadErrorOps(fa)

  implicit final def catsSyntaxMonadErrorRethrow[F[_], E, A](
    fea: F[Either[E, A]]
  )(implicit F: MonadError[F, ? >: E]): MonadErrorRethrowOps[F, E, A] =
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

  def redeemWith[B](recover: E => F[B], bind: A => F[B])(implicit F: MonadError[F, E]): F[B] =
    F.redeemWith[A, B](fa)(recover, bind)

  def attemptTap[B](f: Either[E, A] => F[B])(implicit F: MonadError[F, E]): F[A] =
    F.attemptTap(fa)(f)
}

final class MonadErrorRethrowOps[F[_], E, A](private val fea: F[Either[E, A]]) extends AnyVal {
  def rethrow(implicit F: MonadError[F, ? >: E]): F[A] =
    F.flatMap(fea)(
      _.fold(F.raiseError, F.pure)
    ) // dup from the type class impl, due to https://github.com/scala/bug/issues/11562. Once fixed should be able to replace with `F.rethrow(fea)`
}
