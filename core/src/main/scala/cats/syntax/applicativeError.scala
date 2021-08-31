package cats
package syntax

import cats.data.{EitherT, Validated}

import scala.reflect.ClassTag

trait ApplicativeErrorSyntax {
  implicit final def catsSyntaxApplicativeErrorId[E](e: E): ApplicativeErrorIdOps[E] =
    new ApplicativeErrorIdOps(e)

  implicit final def catsSyntaxApplicativeError[F[_], E, A](
    fa: F[A]
  )(implicit F: ApplicativeError[F, E]): ApplicativeErrorOps[F, E, A] =
    new ApplicativeErrorOps[F, E, A](fa)
}

/**
 * Extension to ApplicativeError in a binary compat way
 */
private[syntax] trait ApplicativeErrorExtension {
  @deprecated("Use methods on ApplicativeError", "2.1.0-RC1")
  final def catsSyntaxApplicativeErrorExtension[F[_], E](
    F: ApplicativeError[F, E]
  ): ApplicativeErrorExtensionOps[F, E] =
    new ApplicativeErrorExtensionOps(F)
}

@deprecated("Use methods on ApplicativeError", "2.1.0-RC1")
final private[syntax] class ApplicativeErrorExtensionOps[F[_], E](F: ApplicativeError[F, E]) {

  @deprecated("Use fromOption on ApplicativeError", "2.1.0-RC1")
  private[syntax] def fromOption[A](oa: Option[A], ifEmpty: => E): F[A] = F.fromOption(oa, ifEmpty)

  @deprecated("Use fromValidated on ApplicativeError", "2.1.0-RC1")
  private[syntax] def fromValidated[A](x: Validated[E, A]): F[A] = F.fromValidated(x)
}

final class ApplicativeErrorIdOps[E](private val e: E) extends AnyVal {
  def raiseError[F[_], A](implicit F: ApplicativeError[F, _ >: E]): F[A] =
    F.raiseError(e)
}

final class ApplicativeErrorOps[F[_], E, A](private val fa: F[A]) extends AnyVal {
  def handleError(f: E => A)(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleError(fa)(f)

  def handleErrorWith(f: E => F[A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleErrorWith(fa)(f)

  def attempt(implicit F: ApplicativeError[F, E]): F[Either[E, A]] =
    F.attempt(fa)

  def attemptNarrow[EE <: Throwable](implicit
    F: ApplicativeError[F, E],
    tag: ClassTag[EE],
    ev: EE <:< E
  ): F[Either[EE, A]] =
    F.attemptNarrow[EE, A](fa)

  def attemptT(implicit F: ApplicativeError[F, E]): EitherT[F, E, A] =
    F.attemptT(fa)

  def recover(pf: PartialFunction[E, A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recover(fa)(pf)

  def recoverWith(pf: PartialFunction[E, F[A]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recoverWith(fa)(pf)

  def redeem[B](recover: E => B, f: A => B)(implicit F: ApplicativeError[F, E]): F[B] =
    F.redeem[A, B](fa)(recover, f)

  def onError(pf: PartialFunction[E, F[Unit]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.onError(fa)(pf)

  def orElse(other: => F[A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleErrorWith(fa)(_ => other)

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
   * scala> "error".asLeft[Int].adaptErr(pf)
   * res0: Either[String,Int] = Left(ERROR)
   *
   * scala> "err".asLeft[Int].adaptErr(pf)
   * res1: Either[String,Int] = Left(err)
   *
   * scala> 1.asRight[String].adaptErr(pf)
   * res2: Either[String,Int] = Right(1)
   * }}}
   *
   * This is the same as `MonadErrorOps#adaptError`. It cannot have the same name because
   * this would result in ambiguous implicits.
   */
  def adaptErr(pf: PartialFunction[E, E])(implicit F: ApplicativeError[F, E]): F[A] = F.adaptError(fa)(pf)

  /**
   * Handle all errors on this F[A] by raising an error using the given value.
   *
   * Example:
   * {{{
   * scala> import cats._, implicits._
   *
   * scala> val fa: Either[String, Int] = Left("wrong")
   *
   * scala> fa.orRaise("wronger")
   * res1: Either[String,Int] = Left(wronger)
   *
   * scala> val fb: Either[String, Int] = Right(42)
   *
   * scala> fb.orRaise("wrongest")
   * res2: Either[String,Int] = Right(42)
   * }}}
   */
  def orRaise(other: => E)(implicit F: ApplicativeError[F, E]): F[A] =
    adaptErr { case _ => other }
}
