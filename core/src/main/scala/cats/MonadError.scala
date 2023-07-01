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

/**
 * A monad that also allows you to raise and or handle an error value.
 *
 * This type class allows one to abstract over error-handling monads.
 */
trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F] {

  /**
   * Turns a successful value into an error if it does not satisfy a given predicate.
   */
  def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A] =
    flatMap(fa)(a => if (predicate(a)) pure(a) else raiseError(error))

  /**
   * Turns a successful value into an error specified by the `error` function if it does not satisfy a given predicate.
   */
  def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A] =
    flatMap(fa)(a => if (predicate(a)) pure(a) else raiseError(error(a)))

  /**
   * Inverse of `attempt`
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import scala.util.{Try, Success}
   *
   * scala> val a: Try[Either[Throwable, Int]] = Success(Left(new java.lang.Exception))
   * scala> a.rethrow
   * res0: scala.util.Try[Int] = Failure(java.lang.Exception)
   *
   * scala> val b: Try[Either[Throwable, Int]] = Success(Right(1))
   * scala> b.rethrow
   * res1: scala.util.Try[Int] = Success(1)
   * }}}
   */
  def rethrow[A, EE <: E](fa: F[Either[EE, A]]): F[A] =
    flatMap(fa)(_.fold(raiseError, pure))

  /**
   * Returns a new value that transforms the result of the source,
   * given the `recover` or `bind` functions, which get executed depending
   * on whether the result is successful or if it ends in error.
   *
   * This is an optimization on usage of [[attempt]] and [[flatMap]],
   * this equivalence being available:
   *
   * {{{
   *   fa.redeemWith(fe, fs) <-> fa.attempt.flatMap(_.fold(fe, fs))
   * }}}
   *
   * Usage of `redeemWith` subsumes [[handleErrorWith]] because:
   *
   * {{{
   *   fa.redeemWith(fe, F.pure) <-> fa.handleErrorWith(fe)
   * }}}
   *
   * Usage of `redeemWith` also subsumes [[flatMap]] because:
   *
   * {{{
   *   fa.redeemWith(F.raiseError, fs) <-> fa.flatMap(fs)
   * }}}
   *
   * Implementations are free to override it in order to optimize
   * error recovery.
   *
   * @see [[redeem]], [[attempt]] and [[handleErrorWith]]
   *
   * @param fa is the source whose result is going to get transformed
   * @param recover is the function that gets called to recover the source
   *        in case of error
   * @param bind is the function that gets to transform the source
   *        in case of success
   */
  def redeemWith[A, B](fa: F[A])(recover: E => F[B], bind: A => F[B]): F[B] =
    flatMap(attempt(fa))(_.fold(recover, bind))

  /**
   * Reifies the value or error of the source and performs an effect on the result,
   * then recovers the original value or error back into `F`.
   *
   * Note that if the effect returned by `f` fails, the resulting effect will fail too.
   *
   * Alias for `fa.attempt.flatTap(f).rethrow` for convenience.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import scala.util.{Try, Success, Failure}
   *
   * scala> def checkError(result: Either[Throwable, Int]): Try[String] = result.fold(_ => Failure(new java.lang.Exception), _ => Success("success"))
   *
   * scala> val a: Try[Int] = Failure(new Throwable("failed"))
   * scala> a.attemptTap(checkError)
   * res0: scala.util.Try[Int] = Failure(java.lang.Exception)
   *
   * scala> val b: Try[Int] = Success(1)
   * scala> b.attemptTap(checkError)
   * res1: scala.util.Try[Int] = Success(1)
   * }}}
   */
  def attemptTap[A, B](fa: F[A])(f: Either[E, A] => F[B]): F[A] =
    rethrow(flatTap(attempt(fa))(f))

  override def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    recoverWith(fa)(pf.andThen(raiseError[A] _))
}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
