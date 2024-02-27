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

import cats.ApplicativeError.CatchOnlyPartiallyApplied
import cats.data.{EitherT, Validated}
import cats.data.Validated.{Invalid, Valid}

import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * An applicative that also allows you to raise and or handle an error value.
 *
 * This type class allows one to abstract over error-handling applicatives.
 */
trait ApplicativeError[F[_], E] extends Applicative[F] {

  /**
   * Lift an error into the `F` context.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * // integer-rounded division
   * scala> def divide[F[_]](dividend: Int, divisor: Int)(implicit F: ApplicativeError[F, String]): F[Int] =
   *      | if (divisor === 0) F.raiseError("division by zero")
   *      | else F.pure(dividend / divisor)
   *
   * scala> type ErrorOr[A] = Either[String, A]
   *
   * scala> divide[ErrorOr](6, 3)
   * res0: ErrorOr[Int] = Right(2)
   *
   * scala> divide[ErrorOr](6, 0)
   * res1: ErrorOr[Int] = Left(division by zero)
   * }}}
   */
  def raiseError[A](e: E): F[A]

  /**
   * Returns `raiseError` when the `cond` is true, otherwise `F.unit`
   *
   * @example {{{
   * val tooMany = 5
   * val x: Int = ???
   * F.raiseWhen(x >= tooMany)(new IllegalArgumentException("Too many"))
   * }}}
   */
  def raiseWhen(cond: Boolean)(e: => E): F[Unit] =
    whenA(cond)(raiseError(e))

  /**
   * Returns `raiseError` when `cond` is false, otherwise `F.unit`
   *
   * @example {{{
   * val tooMany = 5
   * val x: Int = ???
   * F.raiseUnless(x < tooMany)(new IllegalArgumentException("Too many"))
   * }}}
   */
  def raiseUnless(cond: Boolean)(e: => E): F[Unit] =
    unlessA(cond)(raiseError(e))

  /**
   * Handle any error, potentially recovering from it, by mapping it to an
   * `F[A]` value.
   *
   * @see [[handleError]] to handle any error by simply mapping it to an `A`
   * value instead of an `F[A]`.
   *
   * @see [[recoverWith]] to recover from only certain errors.
   */
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  /**
   * Handle any error, by mapping it to an `A` value.
   *
   * @see [[handleErrorWith]] to map to an `F[A]` value instead of simply an
   * `A` value.
   *
   * @see [[recover]] to only recover from certain errors.
   */
  def handleError[A](fa: F[A])(f: E => A): F[A] = handleErrorWith(fa)(f.andThen(pure))

  /**
   * Void any error, by mapping it to `Unit`.
   *
   * This is useful when errors are reported via a side-channel but not directly handled.
   * For example in Cats Effect:
   *
   * {{{
   * IO.deferred[OutcomeIO[A]].flatMap { oc =>
   *   ioa.guaranteeCase(oc.complete(_).void).void.voidError.start
   *   // ...
   * }
   * }}}
   *
   * Without the `.voidError`, the Cats Effect runtime would consider an error in `ioa` to be
   * unhandled and elevate it to [[scala.concurrent.ExecutionContext.reportFailure ExecutionContext#reportFailure]].
   *
   * @see [[handleError]] to map to an `A` value instead of `Unit`.
   * @see [[https://github.com/typelevel/cats-effect/issues/3152 cats-effect#3152]]
   */
  def voidError(fu: F[Unit]): F[Unit] = handleError(fu)(Function.const(()))

  /**
   * Handle errors by turning them into [[scala.util.Either]] values.
   *
   * If there is no error, then an `scala.util.Right` value will be returned instead.
   *
   * All non-fatal errors should be handled by this method.
   */
  def attempt[A](fa: F[A]): F[Either[E, A]] =
    handleErrorWith(
      map(fa)(Right(_): Either[E, A])
    )(e => pure(Left(e)))

  /**
   * Similar to [[attempt]], but wraps the result in a [[cats.data.EitherT]] for
   * convenience.
   */
  def attemptT[A](fa: F[A]): EitherT[F, E, A] = EitherT(attempt(fa))

  /**
   * Similar to [[attempt]], but it only handles errors of type `EE`.
   */
  def attemptNarrow[EE <: Throwable, A](fa: F[A])(implicit tag: ClassTag[EE], ev: EE <:< E): F[Either[EE, A]] =
    recover(map(fa)(Right[EE, A](_): Either[EE, A])) { case e: EE => Left[EE, A](e) }

  /**
   * Recover from certain errors by mapping them to an `A` value.
   *
   * @see [[handleError]] to handle any/all errors.
   *
   * @see [[recoverWith]] to recover from certain errors by mapping them to
   * `F[A]` values.
   */
  def recover[A](fa: F[A])(pf: PartialFunction[E, A]): F[A] =
    handleErrorWith(fa)(e => pf.andThen(pure(_)).applyOrElse(e, raiseError[A](_)))

  /**
   * Recover from certain errors by mapping them to an `F[A]` value.
   *
   * @see [[handleErrorWith]] to handle any/all errors.
   *
   * @see [[recover]] to recover from certain errors by mapping them to `A`
   * values.
   */
  def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    handleErrorWith(fa)(e => pf.applyOrElse(e, raiseError))

  /**
   * Transform certain errors using `pf` and rethrow them.
   * Non matching errors and successful values are not affected by this function.
   *
   * Example:
   * {{{
   * scala> import cats._, implicits._
   *
   * scala> def pf: PartialFunction[String, String] = { case "error" => "ERROR" }
   *
   * scala> ApplicativeError[Either[String, *], String].adaptError("error".asLeft[Int])(pf)
   * res0: Either[String,Int] = Left(ERROR)
   *
   * scala> ApplicativeError[Either[String, *], String].adaptError("err".asLeft[Int])(pf)
   * res1: Either[String,Int] = Left(err)
   *
   * scala> ApplicativeError[Either[String, *], String].adaptError(1.asRight[String])(pf)
   * res2: Either[String,Int] = Right(1)
   * }}}
   *
   * The same function is available in `ApplicativeErrorOps` as `adaptErr` - it cannot have the same
   * name because this would result in ambiguous implicits due to `adaptError`
   * having originally been included in the `MonadError` API and syntax.
   */
  def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    recoverWith(fa)(pf.andThen(raiseError[A] _))

  /**
   * Returns a new value that transforms the result of the source,
   * given the `recover` or `map` functions, which get executed depending
   * on whether the result is successful or if it ends in error.
   *
   * This is an optimization on usage of [[attempt]] and [[map]],
   * this equivalence being available:
   *
   * {{{
   *   fa.redeem(fe, fs) <-> fa.attempt.map(_.fold(fe, fs))
   * }}}
   *
   * Usage of `redeem` subsumes [[handleError]] because:
   *
   * {{{
   *   fa.redeem(fe, id) <-> fa.handleError(fe)
   * }}}
   *
   * Implementations are free to override it in order to optimize
   * error recovery.
   *
   * @see [[MonadError.redeemWith]], [[attempt]] and [[handleError]]
   *
   * @param fa is the source whose result is going to get transformed
   * @param recover is the function that gets called to recover the source
   *        in case of error
   */
  def redeem[A, B](fa: F[A])(recover: E => B, f: A => B): F[B] =
    handleError(map(fa)(f))(recover)

  /**
   * Execute a callback on certain errors, then rethrow them.
   * Any non matching error is rethrown as well.
   *
   * In the following example, only one of the errors is logged,
   * but they are both rethrown, to be possibly handled by another
   * layer of the program:
   *
   * {{{
   * scala> import cats._, data._, implicits._
   *
   * scala> case class Err(msg: String)
   *
   * scala> type F[A] = EitherT[State[String, *], Err, A]
   *
   * scala> val action: PartialFunction[Err, F[Unit]] = {
   *      |   case Err("one") => EitherT.liftF(State.set("one"))
   *      | }
   *
   * scala> val prog1: F[Int] = (Err("one")).raiseError[F, Int]
   * scala> val prog2: F[Int] = (Err("two")).raiseError[F, Int]
   *
   * scala> prog1.onError(action).value.run("").value
   *
   * res0: (String, Either[Err,Int]) = (one,Left(Err(one)))
   *
   * scala> prog2.onError(action).value.run("").value
   * res1: (String, Either[Err,Int]) = ("",Left(Err(two)))
   * }}}
   */
  def onError[A](fa: F[A])(pf: PartialFunction[E, F[Unit]]): F[A] =
    handleErrorWith(fa)(e => pf.andThen(map2(_, raiseError[A](e))((_, b) => b)).applyOrElse(e, raiseError))

  /**
   * Often E is Throwable. Here we try to call pure or catch
   * and raise.
   */
  def catchNonFatal[A](a: => A)(implicit ev: Throwable <:< E): F[A] =
    try pure(a)
    catch {
      case e if NonFatal(e) => raiseError(e)
    }

  /**
   * Often E is Throwable. Here we try to call pure or catch
   * and raise
   */
  def catchNonFatalEval[A](a: Eval[A])(implicit ev: Throwable <:< E): F[A] =
    try pure(a.value)
    catch {
      case e if NonFatal(e) => raiseError(e)
    }

  /**
   * Evaluates the specified block, catching exceptions of the specified type. Uncaught exceptions are propagated.
   */
  def catchOnly[T >: Null <: Throwable]: CatchOnlyPartiallyApplied[T, F, E] =
    new CatchOnlyPartiallyApplied[T, F, E](this)

  /**
   * If the error type is Throwable, we can convert from a scala.util.Try
   */
  def fromTry[A](t: Try[A])(implicit ev: Throwable <:< E): F[A] =
    t match {
      case Success(a) => pure(a)
      case Failure(e) => raiseError(e)
    }

  /**
   * Convert from scala.Either
   *
   * Example:
   * {{{
   * scala> import cats.ApplicativeError
   * scala> import cats.instances.option._
   *
   * scala> ApplicativeError[Option, Unit].fromEither(Right(1))
   * res0: scala.Option[Int] = Some(1)
   *
   * scala> ApplicativeError[Option, Unit].fromEither(Left(()))
   * res1: scala.Option[Nothing] = None
   * }}}
   */
  def fromEither[A](x: E Either A): F[A] =
    x match {
      case Right(a) => pure(a)
      case Left(e)  => raiseError(e)
    }

  /**
   * Convert from scala.Option
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.ApplicativeError
   * scala> val F = ApplicativeError[Either[String, *], String]
   *
   * scala> F.fromOption(Some(1), "Empty")
   * res0: scala.Either[String, Int] = Right(1)
   *
   * scala> F.fromOption(Option.empty[Int], "Empty")
   * res1: scala.Either[String, Int] = Left(Empty)
   * }}}
   */
  def fromOption[A](oa: Option[A], ifEmpty: => E): F[A] =
    oa match {
      case Some(a) => pure(a)
      case None    => raiseError(ifEmpty)
    }

  /**
   * Convert from cats.data.Validated
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.ApplicativeError
   *
   * scala> ApplicativeError[Option, Unit].fromValidated(1.valid[Unit])
   * res0: scala.Option[Int] = Some(1)
   *
   * scala> ApplicativeError[Option, Unit].fromValidated(().invalid[Int])
   * res1: scala.Option[Int] = None
   * }}}
   */
  def fromValidated[A](x: Validated[E, A]): F[A] =
    x match {
      case Invalid(e) => raiseError(e)
      case Valid(a)   => pure(a)
    }
}

object ApplicativeError {
  def apply[F[_], E](implicit F: ApplicativeError[F, E]): ApplicativeError[F, E] = F

  final private[cats] class LiftFromOptionPartially[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](oa: Option[A], ifEmpty: => E)(implicit F: ApplicativeError[F, ? >: E]): F[A] =
      oa match {
        case Some(a) => F.pure(a)
        case None    => F.raiseError(ifEmpty)
      }
  }

  final private[cats] class CatchOnlyPartiallyApplied[T, F[_], E](private val F: ApplicativeError[F, E])
      extends AnyVal {
    def apply[A](f: => A)(implicit CT: ClassTag[T], NT: NotNull[T], ev: Throwable <:< E): F[A] =
      try {
        F.pure(f)
      } catch {
        case t if CT.runtimeClass.isInstance(t) =>
          F.raiseError(t)
      }
  }

  /**
   * lift from scala.Option[A] to a F[A]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.ApplicativeError
   *
   * scala> ApplicativeError.liftFromOption[Either[String, *]](Some(1), "Empty")
   * res0: scala.Either[String, Int] = Right(1)
   *
   * scala> ApplicativeError.liftFromOption[Either[String, *]](Option.empty[Int], "Empty")
   * res1: scala.Either[String, Int] = Left(Empty)
   * }}}
   */
  def liftFromOption[F[_]]: LiftFromOptionPartially[F] = new LiftFromOptionPartially[F]
}
