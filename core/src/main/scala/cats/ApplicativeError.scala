package cats

import cats.data.EitherT

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

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
   * scala> import cats.implicits._
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
  def attemptNarrow[EE, A](fa: F[A])(implicit tag: ClassTag[EE], ev: EE <:< E): F[Either[EE, A]] =
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
    handleErrorWith(fa)(e => (pf.andThen(pure _)).applyOrElse(e, raiseError _))

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
   * @param map is the function that gets to transform the source
   *        in case of success
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

   * res0: (String, Either[Err,Int]) = (one,Left(Err(one)))
   *
   * scala> prog2.onError(action).value.run("").value
   * res1: (String, Either[Err,Int]) = ("",Left(Err(two)))
   * }}}
   */
  def onError[A](fa: F[A])(pf: PartialFunction[E, F[Unit]]): F[A] =
    handleErrorWith(fa)(e => (pf.andThen(map2(_, raiseError[A](e))((_, b) => b))).applyOrElse(e, raiseError))

  /**
   * Often E is Throwable. Here we try to call pure or catch
   * and raise.
   */
  def catchNonFatal[A](a: => A)(implicit ev: Throwable <:< E): F[A] =
    try pure(a)
    catch {
      case NonFatal(e) => raiseError(e)
    }

  /**
   * Often E is Throwable. Here we try to call pure or catch
   * and raise
   */
  def catchNonFatalEval[A](a: Eval[A])(implicit ev: Throwable <:< E): F[A] =
    try pure(a.value)
    catch {
      case NonFatal(e) => raiseError(e)
    }

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

}

object ApplicativeError {
  def apply[F[_], E](implicit F: ApplicativeError[F, E]): ApplicativeError[F, E] = F

  final private[cats] class LiftFromOptionPartially[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](oa: Option[A], ifEmpty: => E)(implicit F: ApplicativeError[F, _ >: E]): F[A] =
      oa match {
        case Some(a) => F.pure(a)
        case None    => F.raiseError(ifEmpty)
      }
  }

  /**
   * lift from scala.Option[A] to a F[A]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
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
