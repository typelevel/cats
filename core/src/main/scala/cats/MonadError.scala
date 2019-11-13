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
   * Transform certain errors using `pf` and rethrow them.
   * Non matching errors and successful values are not affected by this function.
   *
   * Example:
   * {{{
   * scala> import cats._, implicits._
   *
   * scala> def pf: PartialFunction[String, String] = { case "error" => "ERROR" }
   *
   * scala> "error".asLeft[Int].adaptError(pf)
   * res0: Either[String,Int] = Left(ERROR)
   *
   * scala> "err".asLeft[Int].adaptError(pf)
   * res1: Either[String,Int] = Left(err)
   *
   * scala> 1.asRight[String].adaptError(pf)
   * res2: Either[String,Int] = Right(1)
   * }}}
   *
   * The same function is available in `ApplicativeErrorOps` as `adaptErr` - it cannot have the same
   * name because this would result in ambiguous implicits. `adaptError` will be moved from MonadError to
   * ApplicativeError in Cats 2.0: see [[https://github.com/typelevel/cats/issues/2685]]
   */
  def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    recoverWith(fa)(pf.andThen(raiseError[A] _))

  /**
   * Inverse of `attempt`
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
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
   * @see [[redeemWith]], [[attempt]] and [[handleError]]
   *
   * @param fa is the source whose result is going to get transformed
   * @param recover is the function that gets called to recover the source
   *        in case of error
   * @param map is the function that gets to transform the source
   *        in case of success
   */
  def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] =
    redeemWith(fa)(recover.andThen(pure), map.andThen(pure))

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
}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
