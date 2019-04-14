package cats
package syntax

import cats.data.Validated.{Invalid, Valid}
import cats.data.{EitherT, Validated}

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
trait ApplicativeErrorExtension {
  implicit final def catsSyntaxApplicativeErrorExtension[F[_], E](
    F: ApplicativeError[F, E]
  ): ApplicativeErrorExtensionOps[F, E] =
    new ApplicativeErrorExtensionOps(F)
}

final class ApplicativeErrorExtensionOps[F[_], E](F: ApplicativeError[F, E]) {

  /**
   * Convert from scala.Option
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.ApplicativeError
   * scala> val F = ApplicativeError[Either[String, ?], String]
   *
   * scala> F.fromOption(Some(1), "Empty")
   * res0: scala.Either[String, Int] = Right(1)
   *
   * scala> F.fromOption(Option.empty[Int], "Empty")
   * res1: scala.Either[String, Int] = Left(Empty)
   * }}}
   */
  def fromOption[A](oa: Option[A], ifEmpty: => E): F[A] =
    ApplicativeError.liftFromOption(oa, ifEmpty)(F)

  /**
   * Convert from cats.data.Validated
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
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
      case Invalid(e) => F.raiseError(e)
      case Valid(a)   => F.pure(a)
    }

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

  def attemptT(implicit F: ApplicativeError[F, E]): EitherT[F, E, A] =
    F.attemptT(fa)

  def recover(pf: PartialFunction[E, A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recover(fa)(pf)

  def recoverWith(pf: PartialFunction[E, F[A]])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recoverWith(fa)(pf)

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
   * this would result in ambiguous implicits. `adaptError` will be moved from `MonadError`
   * to `ApplicativeError` in Cats 2.0: see [[https://github.com/typelevel/cats/issues/2685]]
   */
  def adaptErr(pf: PartialFunction[E, E])(implicit F: ApplicativeError[F, E]): F[A] =
    F.recoverWith(fa)(pf.andThen(F.raiseError[A] _))
}
