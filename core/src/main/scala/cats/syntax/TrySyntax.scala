package cats.syntax

import cats.Applicative
import cats.data.EitherT

import scala.util.{ Failure, Success, Try }

trait TrySyntax {
  implicit final def catsSyntaxTry[A](t: Try[A]): TryOps[A] = new TryOps[A](t)
}

final class TryOps[A](val t: Try[A]) extends AnyVal {

  /**
    * Converts a `Try[A]` to a `Either[Throwable, A]`.
    *
    * @see [[cats.syntax.EitherObjectOps#fromTry]]
    */
  def toEither: Either[Throwable, A] = t match {
    case Failure(e) => Left(e)
    case Success(v) => Right(v)
  }

  /**
    * Converts a `Try[A]` to a `EitherT[F, Throwable, A]`.
    */
  def toEitherT[F[_]](implicit F: Applicative[F]): EitherT[F, Throwable, A] =
    EitherT.fromEither[F](toEither)
}
