package cats
package data

import cats.instances.either._
import cats.syntax.either._
import cats.internals.EitherUtil.{eitherCast, rightBox, leftBox}

/**
 * Transformer for `Either`, allowing the effect of an arbitrary type
 * constructor `F` to be combined with the fail-fast effect of `Either`.
 *
 * `EitherT[F, A, B]` wraps a value of type `F[Either[A, B]]`.
 *
 * For example `EitherT` can be combined with types such as [[Eval]],
 * `scala.concurrent.Future` or `cats.effect.IO` for principled error
 * handling:
 *
 * {{{
 *   import cats.Eval
 *   import cats.Eval.always
 *   import cats.data.EitherT
 *   import java.lang.Long.{parseNum => javaParseNum}
 *
 *   def parseNum(s: String, radix: Int = 10): EitherT[Eval, NumberFormatException, Long] =
 *     EitherT(always {
 *       try
 *         Right(javaParseNum(s, radix))
 *       catch { case e: NumberFormatException =>
 *         Left(e)
 *       }
 *     })
 * }}}
 *
 * Note that in this function the error type is part of the signature and
 * is more specific than the customary `Throwable` or `Exception`. You can
 * always "upcast" its error type to `Throwable` later using
 * [[Bifunctor.leftWiden]]:
 *
 * {{{
 *   val num: EitherT[Eval, Throwable, Long] =
 *     parseNum("10210", 10).leftWiden
 * }}}
 *
 * The power of `EitherT` is that it combines `F[_]` with the `Either`
 * data type, with the result still being a `MonadError`, so you can
 * comfortably use operations such as [[map]], [[flatMap]], [[attempt]]
 * and others:
 *
 * {{{
 *   def parseEvenNum(s: String, radix: Int = 10): EitherT[Eval, NumberFormatException, Long] =
 *     parseNum(s, radix).flatMap { i =>
 *       if (i % 2 == 0)
 *         EitherT.rightT(i)
 *       else
 *         EitherT.leftT(new NumberFormatException(s"Not an even number: $$i"))
 *     }
 * }}}
 *
 * Tip: An `F[A]` can be lifted in to `EitherT[F, E, A]` via [[EitherT.right]]
 * and lifted in to a `EitherT[F, A, B]` via [[EitherT.left]].
 *
 * {{{
 *   def rand(seed: Long): Eval[Int] =
 *     Eval.always {
 *       val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
 *       (newSeed >>> 16).toInt
 *     }
 *
 *   // Lifting values
 *   def randPredicate(seed: Long)(p: Int => Boolean): EitherT[Eval, String, Int] =
 *     EitherT.right(rand(seed)).flatMap { nr =>
 *       if (p(nr))
 *         EitherT.leftT("Predicate was false")
 *       else
 *         EitherT.pure(nr)
 *     }
 * }}}
 *
 * @define monadErrorF For example data types like `cats.effect.IO` implement
 *         [[MonadError]] and can thus express computations that can fail on their
 *         own, usually via `Throwable`. When working with `EitherT`, if the
 *         underlying `F` is implementing [[ApplicativeError]], sometimes it is
 *         necessary to recover from such errors thrown in `F[_]`.
 */
final case class EitherT[F[_], A, B](value: F[Either[A, B]]) {
  /**
   * Folds the underlying `Either` value, yielding an `F[_]` as result.
   *
   * Example:
   * {{{
   *   val parsed: Eval[Long] = parseNum(num).fold(_ => 0, x => x)
   * }}}
   */
  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] =
    F.map(value)(_.fold(fa, fb))

  /**
   * Queries the underlying `Either` value, returning `true` if it's a `Left`,
   * or `false` otherwise.
   *
   * {{{
   *   // Yields `false` on evaluation
   *   EitherT.rightT[Eval, Throwable](10).isLeft
   *
   *   // Yields `true` on evaluation
   *   EitherT.leftT[Eval, Int]("My error").isLeft
   * }}}
   *
   * @see [[isRight]]
   */
  def isLeft(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isLeft)

  /**
   * Queries the underlying `Either` value, returning `true` if it's a `Right`,
   * or `false` otherwise.
   *
   * {{{
   *   // Yields `true` on evaluation
   *   EitherT.rightT[Eval, String](10).isRight
   *
   *   // Yields `false` on evaluation
   *   EitherT.leftT[Eval, Int]("My error").isRight
   * }}}
   *
   * @see [[isLeft]]
   */
  def isRight(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isRight)

  /**
   * Swaps the left and right values on evaluation, such that if the
   * underlying `Either` is a `Left`, then returns a `Right` on the evaluation
   * of `F[_]` or vice versa.
   *
   * {{{
   *   val rt = EitherT.rightT[Eval, String](10)
   *
   *   rt.value.value
   *   //=> res: Either[String,Int] = Right(10)
   *
   *   rt.swap.value.value
   *   //=> res: Either[Int,String] = Left(10)
   * }}}
   */
  def swap(implicit F: Functor[F]): EitherT[F, B, A] =
    EitherT(F.map(value)(_.swap))

  /**
   * If the underlying `Either` is a `Right`, then returns its value in
   * the `F[_]` context, otherwise returns `default`.
   *
   * {{{
   *   val num1: EitherT[Eval, String, Long] =
   *     EitherT.rightT(10)
   *
   *   // Yields 10
   *   num1.getOrElse(0L)
   *
   *   val num2: EitherT[Eval, String, Long] =
   *     EitherT.leftT("Invalid number")
   *
   *   // Yields 0
   *   num2.getOrElse(0L)
   * }}}
   *
   * @see [[getOrElseF]] and [[orElse]]
   *
   * @param default is a thunk to evaluate in case the underlying
   *        `Either` produced by `F[_]` is a `Left` value
   */
  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] =
    F.map(value)(_.getOrElse(default))

  /**
   * If the underlying `Either` is a `Right`, then returns its value in
   * the `F[_]` context, otherwise evaluate `default` instead.
   *
   * {{{
   *   val num1: EitherT[Eval, String, Long] =
   *     EitherT.rightT(10)
   *
   *   // Yields 10
   *   num1.getOrElseF(Eval.now(0L))
   *
   *   val num2: EitherT[Eval, String, Long] =
   *     EitherT.leftT("Invalid number")
   *
   *   // Yields Long.MaxValue
   *   num2.getOrElseF(Eval.always(Long.MinValue - 1))
   * }}}
   *
   * @see [[getOrElse]] and [[orElse]]
   *
   * @param default is a thunk to evaluate in case the underlying
   *        `Either` produced by `F[_]` is a `Left` value
   */
  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Right(b) => F.pure(b)
      // N.B. pattern match does not use `case Left(_)` on purpose
      case _ => default
    }

  /**
   * Returns the result of the source unchanged if the underlying `Either`
   * is evaluated to a `Right` valued, otherwise evaluates the given
   * `default` thunk.
   *
   * {{{
   *   def randEven(seed: Long): EitherT[Eval, String, Int] =
   *     randPredicate(seed)(_ % 2 == 0) orElse randEven(seed + 1)
   * }}}
   */
  def orElse[AA, BB >: B](default: => EitherT[F, AA, BB])(implicit F: Monad[F]): EitherT[F, AA, BB] =
    EitherT(F.flatMap(value) {
      case r @ Right(_) => F.pure(eitherCast(r))
      // N.B. pattern match does not use `case Left(_)` on purpose
      case _ => default.value
    })

  /**
   * Handles errors by materializing them into `Either` values.
   *
   * This is the implementation of [[ApplicativeError.attempt]].
   *
   * Example:
   * {{{
   *   parseNum(x).attempt.map {
   *     case Right(num) => num
   *     case Left(_) => 0L
   *   }
   * }}}
   *
   * @see [[attemptF]] for recovering errors thrown in `F[_]`
   *
   * @see [[redeem]] and [[redeemWith]] for optimizations on `attempt.map`
   *      and `attempt.flatMap` respectively
   */
  def attempt(implicit F: Functor[F]): EitherT[F, A, Either[A, B]] =
    EitherT.right(value)

  /**
   * Handles errors thrown in `F[_]` (if it implements [[ApplicativeError]]),
   * by materializing them into `Either` values.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *
   *   val rio = EitherT.pure[IO, String](10)
   *
   *   // Yields Right(10)
   *   rio.attemptF
   *
   *   val dummy = new RuntimeException("dummy")
   *   val lio = EitherT[IO, String, Int](IO.raiseError(dummy))
   *
   *   // Yields Left(RuntimeException("dummy"))
   *   lio.attemptF
   * }}}
   *
   * Note that in this sample we are materializing the `Throwable` of the
   * underlying `IO`, even the source `EitherT` value is not parametrized
   * with it.
   *
   * @see [[attempt]] for recovering errors expressed via `EitherT`
   *
   * @see [[redeemF]] and [[redeemWithF]] for optimizations on `attemptF.map`
   *      and `attemptF.flatMap` respectively
   */
  def attemptF[E](implicit F: ApplicativeError[F, E]): EitherT[F, A, Either[E, B]] =
    EitherT(F.map(F.attempt(value)) {
      case Right(eitherB) =>
        eitherB match {
          case right @ Right(_) => Right(eitherCast(right))
          // N.B. pattern match does not do `case Left(_)` on purpose
          case _ => eitherCast(eitherB)
        }
      case error =>
        // N.B. pattern match does not do `case Left(_)` on purpose
        Right(eitherCast(error))
    })

  /**
   * Handle any error, potentially recovering from it, by mapping it via
   * the provided function.
   *
   * This is the implementation of [[ApplicativeError.handleError]].
   *
   * Example:
   * {{{
   *   parseNum(s).handleError(_ => 0L)
   * }}}
   *
   * @see [[handleErrorWith]] for recovering errors by mapping them to
   *      `EitherT` values (aka the equivalent of `flatMap` for errors)
   *
   * @see [[handleErrorF]] for recovering errors thrown in `F[_]`
   */
  def handleError(f: A => B)(implicit F: Functor[F]): EitherT[F, A, B] =
    EitherT(F.map(value) {
      case Left(a) => Right(f(a))
      // N.B. pattern match does not do `case Right(_)` on purpose
      case right => right
    })

  /**
   * Handles any error in `F[_]`, potentially recovering from it, by mapping
   * it via the provided function.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *   import java.lang.Long.{parseNum => javaParseNum}
   *
   *   def parseNum(s: String, r: Int = 10): IO[Long] =
   *     IO(javaParseNum(s, r))
   *
   *   val eio = EitherT.right[String](parseNum("invalid"))
   *
   *   // Yields 0L on evaluation
   *   eio.handleErrorF(_ => 0L)
   * }}}
   *
   * @see [[handleErrorWithF]] for recovering errors by mapping them to
   *      `EitherT` values (aka the equivalent of `flatMap` for errors)
   *
   * @see [[handleError]] for recovering errors expressed via `EitherT`
   */
  def handleErrorF[E](f: E => B)(implicit F: ApplicativeError[F, E]): EitherT[F, A, B] =
    EitherT(F.handleError(value)(e => Right(f(e))))

  /**
   * Handle any error, potentially recovering from it, by mapping it via
   * the provided function to another `EitherT` value.
   *
   * This is the implementation of [[ApplicativeError.handleErrorWith]].
   *
   * Example:
   * {{{
   *   import cats.Eval
   *   import java.lang.Long.{parseNum => javaParseNum}
   *
   *   def parseNum(s: String, r: Int = 10): EitherT[Eval, String, Long] =
   *     EitherT(Eval.always {
   *       try
   *         Right(javaParseNum(s, r))
   *       catch { case _: NumberFormatException =>
   *         Left("invalid number")
   *       }
   *     })
   *
   *   // Yields 10210 because there's no error here
   *   parseNum("10210").handleErrorWith(_ => EitherT.pure(0L))
   *
   *   parseNum("Hello").handleErrorWith {
   *     case "invalid number" =>
   *       EitherT.pure(0L)
   *     case other =>
   *       // Rethrowing error, because we don't know what it is
   *       EitherT.leftT(other)
   *   }
   * }}}
   *
   * @see [[handleError]] for recovering errors by mapping them to simple values
   *
   * @see [[handleErrorWithF]] for recovering errors thrown in `F[_]`
   */
  def handleErrorWith(f: A => EitherT[F, A, B])(implicit F: Monad[F]): EitherT[F, A, B] =
    EitherT(F.flatMap(value) {
      case Left(a) => f(a).value
      // N.B. pattern match does not do `case Right(_)` on purpose
      case right => F.pure(right)
    })

  /**
   * Handles any error in `F[_]`, potentially recovering from it, by mapping
   * it via the provided function to another `EitherT` value.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *   import java.lang.Long.{parseNum => javaParseNum}
   *
   *   def parseNum(s: String, r: Int = 10): IO[Long] =
   *     IO(javaParseNum(s, r))
   *
   *   val eio = EitherT.right[String](parseNum("invalid"))
   *
   *   // Yields 0L on evaluation
   *   eio.handleErrorWithF {
   *     case _: NumberFormatException =>
   *       EitherT.pure(0L)
   *     case other =>
   *       // We are only recovering from NumberFormatException here because we
   *       // don't know of other exceptions that could be thrown and thus we
   *       // prefer to treat them as unrecoverable
   *       EitherT.right(IO.raiseError(other))
   *   }
   * }}}
   *
   * @see [[handleErrorF]] for recovering errors by mapping them to simple values
   *
   * @see [[handleErrorWith]] for recovering errors expressed via `EitherT`
   */
  def handleErrorWithF[E](f: E => EitherT[F, A, B])(implicit F: ApplicativeError[F, E]): EitherT[F, A, B] =
    EitherT(F.handleErrorWith(value)(f(_).value))

  /**
   * Handle any error, potentially recovering from it, by mapping it via
   * the provided partial function. In case the provided partial function
   * isn't defined for the given input, then the error is rethrown.
   *
   * This is the implementation of [[ApplicativeError.recover]].
   *
   * Example:
   * {{{
   *   parseNum(s).recover {
   *     case "invalid number" => 0L
   *   }
   * }}}
   *
   * @see [[handleError]] for handling all errors via a total function
   *
   * @see [[recoverF]] for handling errors thrown in `F[_]`
   */
  def recover(pf: PartialFunction[A, B])(implicit F: Functor[F]): EitherT[F, A, B] =
    EitherT(F.map(value)(_.recover(pf)))

  /**
   * Handles any error in `F[_]`, potentially recovering from it, by mapping
   * it via the provided partial function, with unhandled errors being
   * re-thrown in the `F[_]` context.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *   import java.lang.Long.{parseNum => javaParseNum}
   *
   *   def parseNum(s: String, r: Int = 10): IO[Long] =
   *     IO(javaParseNum(s, r))
   *
   *   val eio = EitherT.right[String](parseNum("invalid"))
   *
   *   // Yields 0L on evaluation
   *   eio.recoverF {
   *     case _: NumberFormatException => 0L
   *   }
   * }}}
   *
   * @see [[handleErrorF]] for recovering all errors via a total function
   *
   * @see [[recover]] for recovering errors expressed via `EitherT`
   */
  def recoverF[E](pf: PartialFunction[E, B])(implicit F: ApplicativeError[F, E]): EitherT[F, A, B] =
    EitherT(F.recover(value)(pf.andThen(rightBox)))

  /**
   * Handle any error, potentially recovering from it, by mapping it via
   * the provided partial function to another `EitherT` value, with
   * unhandled errors being re-thrown in the `EitherT` context.
   *
   * This is the implementation of [[ApplicativeError.recoverWith]].
   *
   * Example:
   * {{{
   *   import cats.Eval
   *   import java.lang.Long.{parseNum => javaParseNum}
   *
   *   def parseNum(s: String, r: Int = 10): EitherT[Eval, String, Long] =
   *     EitherT(Eval.always {
   *       try
   *         Right(javaParseNum(s, r))
   *       catch { case _: NumberFormatException =>
   *         Left("invalid number")
   *       }
   *     })
   *
   *   parseNum("Hello").recoverWith {
   *     case "invalid number" =>
   *       EitherT.pure(0L)
   *   }
   * }}}
   *
   * @see [[handleErrorWith]] for recovering all errors via a total function
   *
   * @see [[recoverWithF]] for recovering errors thrown in `F[_]`
   */
  def recoverWith(pf: PartialFunction[A, EitherT[F, A, B]])(implicit F: Monad[F]): EitherT[F, A, B] =
    EitherT(F.flatMap(value) {
      case Left(a) if pf.isDefinedAt(a) => pf(a).value
      case other => F.pure(other)
    })

  /**
   * Handles errors in `F[_]`, potentially recovering from them, by mapping
   * errors via the provided partial function to other `EitherT` values,
   * unhandled errors being rethrown in the `F[_]` context.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *   import java.lang.Long.{parseNum => javaParseNum}
   *
   *   def parseNum(s: String, r: Int = 10): IO[Long] =
   *     IO(javaParseNum(s, r))
   *
   *   val eio = EitherT.right[String](parseNum("invalid"))
   *
   *   // Yields 0L on evaluation
   *   eio.recoverWithF {
   *     case _: NumberFormatException =>
   *       EitherT.pure(0L)
   *   }
   * }}}
   *
   * @see [[handleErrorWithF]] for recovering errors via a total function
   *
   * @see [[recoverWith]] for recovering errors expressed via `EitherT`
   */
  def recoverWithF[E](pf: PartialFunction[E, EitherT[F, A, B]])(implicit F: ApplicativeError[F, E]): EitherT[F, A, B] =
    EitherT(F.recoverWith(value)(pf.andThen(_.value)))

  /**
   * Given a pair of functions, transforms the underlying `Either` value
   * to a (successful) `Right` result.
   *
   * This is the implementation of [[MonadError.redeem]] and is an operation
   * that can be derived from `attempt.map`, being an optimization on it:
   *
   * {{{
   *   et.redeem(recover, map) <-> et.attempt.map(_.fold(recover, map))
   * }}}
   *
   * Example:
   * {{{
   *   parseNum(x).redeem(
   *     // error recovery
   *     _ => 0L,
   *     num => num
   *   )
   * }}}
   *
   * @see [[attempt]] for materializing errors
   *
   * @see [[redeemF]] for redeeming errors thrown in the `F[_]` context
   */
  def redeem[R](recover: A => R, map: B => R)(implicit F: Functor[F]): EitherT[F, A, R] =
    EitherT(F.map(value) {
      case Right(b) => Right(map(b))
      case Left(a) => Right(recover(a))
    })

  /**
   * Returns a new `EitherT` value that transforms the result of the source,
   * given the `recover` or `map` functions, which get executed depending
   * on whether the underlying `F[_]` is successful or if it ends in error.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *
   *   val rio = EitherT.pure[IO, String](10)
   *
   *   val dummy = new RuntimeException("dummy")
   *   val lio = EitherT[IO, String, Int](IO.raiseError(dummy))
   *
   *   // Yields 0L on evaluation
   *   lio.redeemF(
   *     error => 0L,
   *     num => num)
   * }}}
   *
   * Note that in this sample we are recovering from the `Throwable` of the
   * underlying `IO`, even the source `EitherT` value is not parametrized
   * with it.
   *
   * This function is an optimization on usage of [[attemptF]] and `map`,
   * as this equivalence always holds:
   *
   * {{{
   *   et.redeemF(fe, fb) <-> et.attemptF.map(_.fold(fe, fb))
   * }}}
   *
   * @see [[attemptF]] for materialized errors thrown in the `F[_]` context
   *
   * @see [[redeem]] for recovering errors expressed via `EitherT`
   */
  def redeemF[E, R](recover: E => R, map: B => R)(implicit F: MonadError[F, E]): EitherT[F, A, R] =
    EitherT(F.redeem(value)(e => Right(recover(e)), {
      case Right(a) => Right(map(a))
      case error => eitherCast(error)
    }))

  /**
   * Returns a new value that transforms the result of the source,
   * given the `recover` or `bind` functions, which get executed depending
   * on whether the underlying `Either` value is a `Left` or a `Right`.
   *
   * This is the implementation of [[MonadError.redeemWith]] and is an operation
   * that can be derived from `attempt.flatMap`, being an optimization on it:
   *
   * {{{
   *   et.redeemWith(recover, map) <-> et.attempt.flatMap(_.fold(recover, map))
   * }}}
   *
   * Example:
   * {{{
   *   parseNum(x).redeemWith(
   *     // error recovery
   *     error => error match {
   *       case "invalid number" => EitherT.pure(0L)
   *       case other =>
   *         // Rethrowing unknown error types
   *         EitherT.leftT(other)
   *     },
   *     num =>
   *       // Binding to another value; we could do something more
   *       // fancy here, like an actual bind continuation
   *       EitherT.pure(num)
   *   )
   * }}}
   *
   * @see [[attempt]] for materializing errors
   *
   * @see [[redeemWithF]] for redeeming errors thrown in the `F[_]` context
   */
  def redeemWith[R](recover: A => EitherT[F, A, R], bind: B => EitherT[F, A, R])(implicit F: Monad[F]): EitherT[F, A, R] =
    EitherT(F.flatMap(value) {
      case Right(b) => bind(b).value
      case Left(a) => recover(a).value
    })

  /**
   * Returns a new `EitherT` value that transforms the result of the source,
   * given the `recover` or `bind` functions, which get executed depending
   * on whether the underlying `F[_]` is successful or if it ends in error.
   *
   * $monadErrorF
   *
   * Example:
   * {{{
   *   import cats.effect.IO
   *
   *   val rio = EitherT.pure[IO, String](10)
   *
   *   val dummy = new RuntimeException("dummy")
   *   val lio = EitherT[IO, String, Int](IO.raiseError(dummy))
   *
   *   // Yields 0L on evaluation
   *   lio.redeemWithF(
   *     // error recovery
   *     error => error match {
   *       case `dummy` => EitherT.pure(0L)
   *       case other =>
   *         // Rethrowing other errors we don't recognize
   *         EitherT(IO.raiseError(other))
   *     },
   *     num => {
   *       // Binding to another value; we could do something more
   *       // fancy here, like an actual bind continuation
   *       EitherT.pure(num)
   *     })
   * }}}
   *
   * Note that in this sample we are recovering from the `Throwable` of the
   * underlying `IO`, even the source `EitherT` value is not parametrized
   * with it.
   *
   * This function is an optimization on usage of [[attemptF]] and `flatMap`,
   * as this equivalence always holds:
   *
   * {{{
   *   et.redeemWithF(fe, fb) <-> et.attemptF.flatMap(_.fold(fe, fb))
   * }}}
   *
   * @see [[attemptF]] for materialized errors thrown in the `F[_]` context
   *
   * @see [[redeemWith]] for recovering errors expressed via `EitherT`
   */
  def redeemWithF[E, R](recover: E => EitherT[F, A, R], bind: B => EitherT[F, A, R])(implicit F: MonadError[F, E]): EitherT[F, A, R] =
    EitherT(F.redeemWith(value)(e => recover(e).value, {
      case Right(a) => bind(a).value
      case error => F.pure(eitherCast(error))
    }))

  /**
   * Returns the `Right` value in `F[_]`, or transforms the `Left` value
   * via the provided function.
   *
   * {{{
   *   val et1 = EitherT.right[String](Eval.always(10))
   *   // Yields 10
   *   et1.valueOr(_ => 0L)
   *
   *   val et2 = EitherT.left[int](Eval.always("error"))
   *   // Yields 0
   *   et2.valueOr(_ => 0L)
   * }}}
   */
  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F]): F[BB] =
    fold(f, identity)

  /**
   * Returns the `Right` value in `F[_]`, or transforms the `Left` value
   * via the provided function that processes the final result in `F[_]`.
   *
   * {{{
   *   val et1 = EitherT.right[String](Eval.always(10))
   *   // Yields 10
   *   et1.valueOrF(_ => parseNum("0"))
   *
   *   val et2 = EitherT.left[int](Eval.always("error"))
   *   // Yields 0
   *   et2.valueOrF(_ => parseNum("0"))
   * }}}
   */
  def valueOrF[BB >: B](f: A => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value){
      case Left(a) => f(a)
      case Right(b) => F.pure(b)
    }

  /**
   * Returns `true` if `Left` or returns the result of the application of
   * the given predicate function to the `Right` value.
   */
  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.forall(f))

  /**
   * Returns `false` if `Left` or returns the result of the given predicate
   * function applied to the `Right` value.
   */
  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.exists(f))

  /**
   * Turns a successful value into an error if it does not satisfy the given
   * predicate.
   *
   * This is the implementation of [[MonadError.ensure]].
   */
  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
    EitherT(F.map(value)(_.ensure(onFailure)(f)))

  /**
   * Turns a successful value into an error specified by the `onFailure`
   * function if it does not satisfy a given predicate.
   *
   * This is the implementation of [[MonadError.ensureOr]].
   */
  def ensureOr[AA >: A](onFailure: B => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
    EitherT(F.map(value)(_.ensureOr(onFailure)(f)))

  /**
   * Converts this `EitherT` to [[OptionT]], `Right` values being converted
   * to `Some` and `Left` values being converted to `None`.
   */
  def toOption(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.toOption))

  /**
   * Given a `G[_]` type parameter that implements [[Alternative]], converts
   * this `EitherT` value to it, keeping the `F[_]` context.
   *
   * `Alternative` is basically [[MonoidK]] with [[Applicative]], meaning
   * that `Left` values get translated to [[MonoidK.empty]] and `Right` values
   * get translated to [[Applicative.pure]].
   *
   * For example:
   *
   * {{{
   *   val num = EitherT.right[String](Eval.always(1))
   *
   *   val numOpt: Eval[Option[Int]] = num.to[Option]
   *   numOpt.value
   *   //=> Some(1)
   *
   *   val numList: Eval[List[Int]] = num.to[List]
   *   numList.value
   *   //=> List(1)
   *
   *   val err = EitherT.left[Int](Eval.always("error"))
   *
   *   val errOpt: Eval[Option[Int]] = err.to[Option]
   *   errOpt.value
   *   //=> None
   *
   *   val errList: Eval[List[Int]] = err.to[List]
   *   errList.value
   *   //=> List()
   * }}}
   */
  def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[B]] =
    F.map(value)(_.to[G])

  /**
   * Given ab `F[_]` that implements [[Alternative]], collect all
   * `Right` values or convert `Left` values to [[MonoidK.empty empty]].
   *
   * Example:
   * {{{
   *   def validateNum(num: Int): Either[String, Int] =
   *     if (num % 2 == 0) Right(num)
   *     else Left("number is odd")
   *
   *   val et: EitherT[List, String, Int] =
   *     EitherT((0 until 1000).toList.map(validateNum))
   *
   *   // Yields 0, 2, 4 ... 98
   *   val evens: List[Int] = et.collectRight
   * }}}
   */
  def collectRight(implicit FA: Alternative[F], FM: Monad[F]): F[B] =
    FM.flatMap(value)(_.to[F])

  /**
   * The `bimap` operation can apply a transformation to each "side" of
   * the underlying `Either` value.
   *
   * This is the [[Bifunctor.bimap]] implementation. It's very much like
   * normal [[map]], except that it can also transform the left side as well.
   *
   * Example:
   * {{{
   *   val et1: EitherT[Eval, String, Int] =
   *     EitherT.leftT("12012")
   *
   *   // Yields Left(12012 : Int)
   *   val et2: EitherT[Eval, Int, String] =
   *     et1.bimap(
   *       left => left.toInt,
   *       right => right.toString)
   *
   *   // Yields Left(23123)
   *   val et3: EitherT[Eval, Int, Int] =
   *     et2.bimap(a => a + 11111, _.toInt)
   * }}}
   *
   * @param fa is the mapping function applied to `Left` values
   * @param fb is the mapping function applied to `Right` values
   */
  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(_.bimap(fa, fb)))

  /**
   * Traverse each "side" of the structure with the given functions.
   *
   * Implements [[Bitraverse.bitraverse]].
   */
  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[EitherT[F, C, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Bitraverse[Either].bitraverse(axb)(f, g)))(EitherT.apply)

  /**
   * Given a mapping function in the `EitherT[F, A, ?]` context, applies it
   * to `Right` values.
   *
   * This is very much like [[Applicative.ap]], except it's made to work with
   * the right side, as needed by a bifunctor like `EitherT`.
   */
  def applyAlt[D](ff: EitherT[F, A, B => D])(implicit F: Apply[F]): EitherT[F, A, D] =
    EitherT[F, A, D](F.map2(this.value, ff.value)((xb, xbd) => Apply[Either[A, ?]].ap(xbd)(xb)))

  /**
   * The monadic bind.
   *
   * Implements [[FlatMap.flatMap]].
   *
   * Monad transforms like `EitherT` are essentially built to provide a `flatMap`
   * implementation that reuses the underlying `F.flatMap` operation, hiding
   * the complexity involved (in this case hiding the plumbing needed for
   * dealing with `Left` values).
   *
   * @see [[flatMapF]] for mapping functions returning values in the
   *      `F[_]` context
   */
  def flatMap[AA >: A, D](f: B => EitherT[F, AA, D])(implicit F: Monad[F]): EitherT[F, AA, D] =
    EitherT(F.flatMap(value) {
      case Right(b) => f(b).value
      // N.B. pattern match doesn't do `case Left(_)` on purpose
      case left => F.pure(eitherCast(left))
    })

  /**
   * A monadic bind variant that takes a parameter a mapping function that
   * works with `F[_]` return types, instead of `EitherT`.
   *
   * It has the same properties as normal [[flatMap]], except that it accepts
   * functions whose output isn't wrapped in `EitherT` already.
   */
  def flatMapF[AA >: A, D](f: B => F[Either[AA, D]])(implicit F: Monad[F]): EitherT[F, AA, D] =
    flatMap(b => EitherT(f(b)))

  /**
   * Maps the underlying `F[_]` value with the provided function.
   *
   * This is a shorthand for mapping the [[value]] directly:
   * {{{
   *   et.transform(f) <-> EitherT(et.value.map(f))
   * }}}
   */
  def transform[C, D](f: Either[A, B] => Either[C, D])(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(f))

  /**
   * Applies `flatMap` to the underlying `Either` value.
   *
   * Notice the equivalence:
   * {{{
   *   et.subflatMap(f) <-> EitherT(et.value.map(_.flatMap(f)))
   * }}}
   */
  def subflatMap[AA >: A, D](f: B => Either[AA, D])(implicit F: Functor[F]): EitherT[F, AA, D] =
    transform(_.flatMap(f))

  /**
   * Given the mapping function, returns a new `EitherT` that transform
   * the `Right` values of the source with it.
   *
   * Implements [[Functor.map]].
   */
  def map[D](f: B => D)(implicit F: Functor[F]): EitherT[F, A, D] =
    EitherT(F.map(value) {
      case Right(b) => Right(f(b))
      case left => eitherCast(left)
    })

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): EitherT[G, A, B] = EitherT[G, A, B](f(value))

  /**
   * Maps `Right` values generated by the source using the provided function,
   * whose output is given in the `F[_]` context.
   *
   * This operation resembles [[flatMap]] or [[flatMapF]], except that the
   * returned value is wrapped via [[EitherT.right]] so it lacks the ability
   * of yielding `Left` values, being more like [[map]] in that sense.
   *
   * Example:
   * {{{
   *   def parseInt(n: String): EitherT[Eval, String, Int] =
   *     EitherT(Eval.always {
   *       try Right(n.toInt)
   *       catch { case NonFatal(_) => Left("invalid number") }
   *     })
   *
   *   // Yields Right(10000) on evaluation
   *   parseInt("9999").semiflatMap { n =>
   *     Eval.always(n + 1)
   *   }
   * }}}
   */
  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): EitherT[F, A, D] =
    flatMap(b => EitherT.right(f(b)))

  /**
   * Applies the mapping function to the left "side".
   *
   * Like [[map]], except that it operates on `Left` values.
   *
   * @see [[bimap]] for mapping both sides
   */
  def leftMap[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] =
    bimap(f, identity)

  /**
   * Binds the source to a new `EitherT` value using the provided function
   * that operates on the left "side".
   *
   * This is like [[flatMap]] except that it operates on `Left` values,
   * needed because `EitherT` is a bifunctor.
   */
  def leftFlatMap[BB >: B, D](f: A => EitherT[F, D, BB])(implicit F: Monad[F]): EitherT[F, D, BB] =
    EitherT(F.flatMap(value) {
      case Left(a) => f(a).value
      // N.B. pattern match doesn't do `case Right(_)` on purpose
      case right => F.pure(eitherCast(right))
    })

  /**
   * Maps `Left` values generated by the source using the provided function,
   * whose output is given in the `F[_]` context.
   *
   * This is like [[semiflatMap]] except that it operates on `Left` values,
   * needed because `EitherT` is a bifunctor.
   */
  def leftSemiflatMap[D](f: A => F[D])(implicit F: Monad[F]): EitherT[F, D, B] =
    EitherT(F.flatMap(value) {
      case Left(a) => F.map(f(a)) { d => Left(d) }
      // N.B. pattern match doesn't do `case Right(_)` on purpose
      case right => F.pure(eitherCast(right))
    })

  def compare(that: EitherT[F, A, B])(implicit o: Order[F[Either[A, B]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: EitherT[F, A, B])(implicit p: PartialOrder[F[Either[A, B]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: EitherT[F, A, B])(implicit eq: Eq[F[Either[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[EitherT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Traverse[Either[A, ?]].traverse(axb)(f)))(EitherT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb.foldLeft(c)(f))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((axb, lc) => axb.foldRight(lc)(f))

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] =
    F.map(value)(_.fold(identity, ev.apply))

  /**
   * Similar to `Either#combine` but mapped over an `F` context.
   *
   * Examples:
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val l1: EitherT[Option, String, Int] = EitherT.left(Some("error 1"))
   * scala> val l2: EitherT[Option, String, Int] = EitherT.left(Some("error 2"))
   * scala> val r3: EitherT[Option, String, Int] = EitherT.right(Some(3))
   * scala> val r4: EitherT[Option, String, Int] = EitherT.right(Some(4))
   * scala> val noneEitherT: EitherT[Option, String, Int] = EitherT.left(None)
   *
   * scala> l1 combine l2
   * res0: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> l1 combine r3
   * res1: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> r3 combine l1
   * res2: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> r3 combine r4
   * res3: EitherT[Option, String, Int] = EitherT(Some(Right(7)))
   *
   * scala> l1 combine noneEitherT
   * res4: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> noneEitherT combine l1
   * res5: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> r3 combine noneEitherT
   * res6: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> noneEitherT combine r4
   * res7: EitherT[Option, String, Int] = EitherT(None)
   * }}}
   */
  def combine(that: EitherT[F, A, B])(implicit F: Apply[F], B: Semigroup[B]): EitherT[F, A, B] =
    EitherT(F.map2(this.value, that.value)(_ combine _))

  def toValidated(implicit F: Functor[F]): F[Validated[A, B]] =
    F.map(value)(_.toValidated)

  def toValidatedNel(implicit F: Functor[F]): F[ValidatedNel[A, B]] =
    F.map(value)(_.toValidatedNel)

  /** Run this value as a `[[Validated]]` against the function and convert it back to an `[[EitherT]]`.
   *
   * The [[Applicative]] instance for `EitherT` "fails fast" - it is often useful to "momentarily" have
   * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type Error = String
   * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 1")
   * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 2")
   * scala> val eithert: EitherT[Option, Error, Int] = EitherT.leftT[Option, Int]("error 3")
   * scala> eithert.withValidated { v3 => (v1, v2, v3.toValidatedNel).mapN { case (i, j, k) => i + j + k } }
   * res0: EitherT[Option, NonEmptyList[Error], Int] = EitherT(Some(Left(NonEmptyList(error 1, error 2, error 3))))
   * }}}
   */
  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB])(implicit F: Functor[F]): EitherT[F, AA, BB] =
    EitherT(F.map(value)(either => f(either.toValidated).toEither))

  def show(implicit show: Show[F[Either[A, B]]]): String =
    show.show(value)

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Either[A, ?], B]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Either[A, ?]`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `EitherT`.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val ff: EitherT[List, String, Int => String] =
   *      |   EitherT(List(Either.right(_.toString), Either.left("error")))
   * scala> val fa: EitherT[List, String, Int] =
   *      |   EitherT(List(Either.right(1), Either.right(2)))
   * scala> ff.ap(fa)
   * res0: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error)))
   * scala> EitherT((ff.toNested).ap(fa.toNested).value)
   * res1: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error), Left(error)))
   * }}}
   *
   */
  def toNested: Nested[F, Either[A, ?], B] =
    Nested[F, Either[A, ?], B](value)

  /**
    * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Validated[A, ?], B]`.
    *
    * Example:
    * {{{
    * scala> import cats.data.{EitherT, Validated}
    * scala> import cats.implicits._
    * scala> val f: Int => String = i => (i*2).toString
    * scala> val r1: EitherT[Option, String, Int => String] = EitherT.right(Some(f))
    * r1: cats.data.EitherT[Option,String,Int => String] = EitherT(Some(Right(<function1>)))
    * scala> val r2: EitherT[Option, String, Int] = EitherT.right(Some(10))
    * r2: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(10)))
    * scala> type ErrorOr[A] = Validated[String, A]
    * scala> (r1.toNestedValidated).ap(r2.toNestedValidated)
    * res0: cats.data.Nested[Option,ErrorOr,String] = Nested(Some(Valid(20)))
    * }}}
    */
  def toNestedValidated(implicit F: Functor[F]): Nested[F, Validated[A, ?], B] =
    Nested[F, Validated[A, ?], B](F.map(value)(_.toValidated))

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNel[A, ?], B]`.
   */
  def toNestedValidatedNel(implicit F: Functor[F]): Nested[F, ValidatedNel[A, ?], B] =
    Nested[F, ValidatedNel[A, ?], B](F.map(value)(_.toValidatedNel))
}

object EitherT extends EitherTInstances {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class LeftPartiallyApplied[B](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fa)(leftBox))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `F[A]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.left[Int](Option("err"))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class LeftTPartiallyApplied[F[_], B](val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): EitherT[F, A, B] = EitherT(F.pure(Either.left(a)))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `A`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.leftT[Option, Int]("err")
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class RightPartiallyApplied[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fb)(rightBox))
  }

  /**
   * Creates a right version of `EitherT[F, A, B]` from a `F[B]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.right[String](Option(3))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class PurePartiallyApplied[F[_], A](val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: Applicative[F]): EitherT[F, A, B] = right(F.pure(b))
  }

  /**
   * Creates a new `EitherT[F, A, B]` from a `B`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.pure[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

  /**
   * Alias for [[pure]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.rightT[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure


  /**
   * Alias for [[right]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(3)
   * scala> val n: Option[Int] = None
   * scala> EitherT.liftF(o)
   * res0: cats.data.EitherT[Option,Nothing,Int] = EitherT(Some(Right(3)))
   * scala> EitherT.liftF(n)
   * res1: cats.data.EitherT[Option,Nothing,Int] = EitherT(None)
   * }}}
   */
  final def liftF[F[_], A, B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = right(fb)

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, ?]]
   * scala> val b: OptionT[EitherT[Eval, String, ?], Int] = a.mapK(EitherT.liftK)
   * scala> b.value.value.value
   * res0: Either[String,Option[Int]] = Right(Some(1))
   * }}}
   */
  final def liftK[F[_], A](implicit F: Functor[F]): F ~> EitherT[F, A, ?] =
    λ[F ~> EitherT[F, A, ?]](right(_))

  @deprecated("Use EitherT.liftF.", "1.0.0-RC1")
  final def liftT[F[_], A, B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = right(fb)

  /** Transforms an `Either` into an `EitherT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromEitherPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromEither like this:
   * {{{
   * scala> import cats.implicits._
   * scala> val t: Either[String, Int] = Either.right(3)
   * scala> EitherT.fromEither[Option](t)
   * res0: EitherT[Option, String, Int] = EitherT(Some(Right(3)))
   * }}}
   *
   * The reason for the indirection is to emulate currying type parameters.
   */
  final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromEitherPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(either))
  }

  /** Transforms an `Option` into an `EitherT`, lifted into the specified `Applicative` and using
   *  the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOption[List](o, "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOption[List](Some(42), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromOptionPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](opt: Option[A], ifNone: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(Either.fromOption(opt, ifNone)))
  }

  /** Transforms an `F[Option]` into an `EitherT`, using the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOptionF(List(o), "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOptionF(List(Option(42)), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOptionF[F[_], E, A](fopt: F[Option[A]], ifNone: => E)(implicit F: Functor[F]): EitherT[F, E, A] =
    EitherT(F.map(fopt)(opt => Either.fromOption(opt, ifNone)))

  /**  If the condition is satisfied, return the given `A` in `Right`
    *  lifted into the specified `Applicative`, otherwise, return the
    *  given `E` in `Left` lifted into the specified `Applicative`.
    *
    * {{{
    * scala> import cats.Id
    * scala> import cats.data.EitherT
    * scala> val userInput = "hello world"
    * scala> EitherT.cond[Id](
    *      |   userInput.forall(_.isDigit) && userInput.size == 10,
    *      |   userInput,
    *      |   "The input does not look like a phone number")
    * res0: EitherT[Id, String, String] = EitherT(Left(The input does not look like a phone number))
    * }}}
    */
  final def cond[F[_]]: CondPartiallyApplied[F] = new CondPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class CondPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](test: Boolean, right: => A, left: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(Either.cond(test, right, left)))
  }
}

private[data] abstract class EitherTInstances extends EitherTInstances1 {

  implicit def catsDataOrderForEitherT[F[_], L, R](implicit F: Order[F[Either[L, R]]]): Order[EitherT[F, L, R]] =
    new EitherTOrder[F, L, R] {
      val F0: Order[F[Either[L, R]]] = F
    }

  implicit def catsDataShowForEitherT[F[_], L, R](implicit sh: Show[F[Either[L, R]]]): Show[EitherT[F, L, R]] =
    Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForEitherT[F[_]](implicit F: Functor[F]): Bifunctor[EitherT[F, ?, ?]] =
    new Bifunctor[EitherT[F, ?, ?]] {
      override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: A => C, g: B => D): EitherT[F, C, D] = fab.bimap(f, g)
    }

  implicit def catsDataTraverseForEitherT[F[_], L](implicit F: Traverse[F]): Traverse[EitherT[F, L, ?]] =
    new EitherTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  implicit def catsMonoidForEitherT[F[_], L, A](implicit F: Monoid[F[Either[L, A]]]): Monoid[EitherT[F, L, A]] =
    new EitherTMonoid[F, L, A] { implicit val F0 = F }

}

private[data] abstract class EitherTInstances1 extends EitherTInstances2 {

  implicit def catsSemigroupForEitherT[F[_], L, A](implicit F: Semigroup[F[Either[L, A]]]): Semigroup[EitherT[F, L, A]] =
    new EitherTSemigroup[F, L, A] { implicit val F0 = F }

  implicit def catsDataFoldableForEitherT[F[_], L](implicit F: Foldable[F]): Foldable[EitherT[F, L, ?]] =
    new EitherTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def catsDataPartialOrderForEitherT[F[_], L, R](implicit F: PartialOrder[F[Either[L, R]]]): PartialOrder[EitherT[F, L, R]] =
    new EitherTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[Either[L, R]]] = F
    }

  implicit def catsDataBitraverseForEitherT[F[_]](implicit F: Traverse[F]): Bitraverse[EitherT[F, ?, ?]] =
    new EitherTBitraverse[F] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataMonadErrorForEitherT[F[_], L](implicit F0: Monad[F]): MonadError[EitherT[F, L, ?], L] =
    new EitherTMonadError[F, L] {
      implicit val F = F0
      override def ensure[A](fa: EitherT[F, L, A])(error: => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensure(error)(predicate)(F)

      override def ensureOr[A](fa: EitherT[F, L, A])(error: (A) => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensureOr(error)(predicate)(F)
    }
}

private[data] abstract class EitherTInstances2 extends EitherTInstances3 {
  /**  Monad error instance for recovering errors in F instead of
   *  the underlying Either.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.MonadError
   * scala> import cats.instances.option._
   * scala> val noInt: Option[Either[String, Int]] = None
   * scala> val et = EitherT[Option, String, Int](noInt)
   * scala> val me = MonadError[EitherT[Option, String, ?], Unit]
   * scala> me.recover(et) { case () => 1 }
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(1)))
   * }}}
   */
  implicit def catsDataMonadErrorFForEitherT[F[_], E, L](implicit FE0: MonadError[F, E]): MonadError[EitherT[F, L, ?], E] =
    new EitherTMonadErrorF[F, E, L] { implicit val F = FE0 }


  implicit def catsDataSemigroupKForEitherT[F[_], L](implicit F0: Monad[F]): SemigroupK[EitherT[F, L, ?]] =
    new EitherTSemigroupK[F, L] { implicit val F = F0 }

  implicit def catsDataEqForEitherT[F[_], L, R](implicit F: Eq[F[Either[L, R]]]): Eq[EitherT[F, L, R]] =
    new EitherTEq[F, L, R] {
      val F0: Eq[F[Either[L, R]]] = F
    }
}

private[data] abstract class EitherTInstances3 {
  implicit def catsDataFunctorForEitherT[F[_], L](implicit F0: Functor[F]): Functor[EitherT[F, L, ?]] =
    new EitherTFunctor[F, L] { implicit val F = F0 }
}

private[data] trait EitherTSemigroup[F[_], L, A] extends Semigroup[EitherT[F, L, A]] {
  implicit val F0: Semigroup[F[Either[L, A]]]
  def combine(x: EitherT[F, L , A], y: EitherT[F, L , A]): EitherT[F, L , A] =
    EitherT(F0.combine(x.value, y.value))
}

private[data] trait EitherTMonoid[F[_], L, A] extends Monoid[EitherT[F, L, A]] with EitherTSemigroup[F, L, A] {
  implicit val F0: Monoid[F[Either[L, A]]]
  def empty: EitherT[F, L, A] = EitherT(F0.empty)
}

private[data] trait EitherTSemigroupK[F[_], L] extends SemigroupK[EitherT[F, L, ?]] {
  implicit val F: Monad[F]
  def combineK[A](x: EitherT[F, L, A], y: EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(x.value) {
      case l @ Left(_) => y.value
      case r @ Right(_) => F.pure(r)
    })
}

private[data] trait EitherTFunctor[F[_], L] extends Functor[EitherT[F, L, ?]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = fa map f
}

private[data] trait EitherTMonad[F[_], L] extends Monad[EitherT[F, L, ?]] with EitherTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): EitherT[F, L, A] = EitherT.pure(a)

  def flatMap[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] = fa flatMap f
  def tailRecM[A, B](a: A)(f: A => EitherT[F, L, Either[A, B]]): EitherT[F, L, B] =
    EitherT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
      case Left(l)         => Right(Left(l))
      case Right(Left(a1)) => Left(a1)
      case Right(Right(b)) => Right(Right(b))
    }))
}

private[data] trait EitherTMonadErrorF[F[_], E, L] extends MonadError[EitherT[F, L, ?], E] with EitherTMonad[F, L] {
  implicit val F: MonadError[F, E]

  override def handleError[A](fea: EitherT[F, L, A])(f: E => A): EitherT[F, L, A] =
    fea.handleErrorF(f)
  override def handleErrorWith[A](fea: EitherT[F, L, A])(f: E => EitherT[F, L, A]): EitherT[F, L, A] =
    fea.handleErrorWithF(f)
  override def attempt[A](fea: EitherT[F, L, A]): EitherT[F, L, Either[E, A]] =
    fea.attemptF
  override def redeem[A, B](fea: EitherT[F, L, A])(fe: E => B, fs: A => B): EitherT[F, L, B] =
    fea.redeemF(fe, fs)
  override def redeemWith[A, B](fea: EitherT[F, L, A])(fe: E => EitherT[F, L, B], fs: A => EitherT[F, L, B]): EitherT[F, L, B] =
    fea.redeemWithF(fe, fs)
  override def raiseError[A](e: E): EitherT[F, L, A] =
    EitherT(F.raiseError(e))
}

private[data] trait EitherTMonadError[F[_], L] extends MonadError[EitherT[F, L, ?], L] with EitherTMonad[F, L] {
  override def handleError[A](fla: EitherT[F, L, A])(f: L => A): EitherT[F, L, A] =
    fla.handleError(f)
  def handleErrorWith[A](fla: EitherT[F, L, A])(f: L => EitherT[F, L, A]): EitherT[F, L, A] =
    fla.handleErrorWith(f)
  override def redeem[A, B](fla: EitherT[F, L, A])(recover: L => B, map: A => B): EitherT[F, L, B] =
    fla.redeem(recover, map)
  override def redeemWith[A, B](fla: EitherT[F, L, A])(recover: L => EitherT[F, L, B], bind: A => EitherT[F, L, B]): EitherT[F, L, B] =
    fla.redeemWith(recover, bind)
  override def raiseError[A](e: L): EitherT[F, L, A] =
    EitherT(F.pure(Left(e)))
  override def attempt[A](fla: EitherT[F, L, A]): EitherT[F, L, Either[L, A]] =
    fla.attempt
  override def recover[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, A]): EitherT[F, L, A] =
    fla.recover(pf)
  override def recoverWith[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, EitherT[F, L, A]]): EitherT[F, L, A] =
    fla.recoverWith(pf)
}

private[data] sealed trait EitherTFoldable[F[_], L] extends Foldable[EitherT[F, L, ?]] {
  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: EitherT[F, L, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: EitherT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

private[data] sealed trait EitherTTraverse[F[_], L] extends Traverse[EitherT[F, L, ?]] with EitherTFoldable[F, L] {
  override implicit def F0: Traverse[F]

  override def traverse[G[_]: Applicative, A, B](fa: EitherT[F, L, A])(f: A => G[B]): G[EitherT[F, L, B]] =
    fa traverse f
}

private[data] sealed trait EitherTBifoldable[F[_]] extends Bifoldable[EitherT[F, ?, ?]] {
  implicit def F0: Foldable[F]

  def bifoldLeft[A, B, C](fab: EitherT[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F0.foldLeft(fab.value, c)( (acc, axb) => Bifoldable[Either].bifoldLeft(axb, acc)(f, g))

  def bifoldRight[A, B, C](fab: EitherT[F, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F0.foldRight(fab.value, c)( (axb, acc) => Bifoldable[Either].bifoldRight(axb, acc)(f, g))
}

private[data] sealed trait EitherTBitraverse[F[_]] extends Bitraverse[EitherT[F, ?, ?]] with EitherTBifoldable[F] {
  override implicit def F0: Traverse[F]

  override def bitraverse[G[_], A, B, C, D](fab: EitherT[F, A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

private[data] sealed trait EitherTEq[F[_], L, A] extends Eq[EitherT[F, L, A]] {
  implicit def F0: Eq[F[Either[L, A]]]

  override def eqv(x: EitherT[F, L, A], y: EitherT[F, L, A]): Boolean = x === y
}

private[data] sealed trait EitherTPartialOrder[F[_], L, A] extends PartialOrder[EitherT[F, L, A]] with EitherTEq[F, L, A]{
  override implicit def F0: PartialOrder[F[Either[L, A]]]

  override def partialCompare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Double =
    x partialCompare y
}

private[data] sealed trait EitherTOrder[F[_], L, A] extends Order[EitherT[F, L, A]] with EitherTPartialOrder[F, L, A]{
  override implicit def F0: Order[F[Either[L, A]]]

  override def compare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Int = x compare y
}
