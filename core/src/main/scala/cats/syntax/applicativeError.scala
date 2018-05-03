package cats
package syntax

import cats.data.EitherT

trait ApplicativeErrorSyntax {
  implicit final def catsSyntaxApplicativeErrorId[E](e: E): ApplicativeErrorIdOps[E] =
    new ApplicativeErrorIdOps(e)

  implicit final def catsSyntaxApplicativeError[F[_], E, A](fa: F[A])(implicit F: ApplicativeError[F, E]): ApplicativeErrorOps[F, E, A] =
    new ApplicativeErrorOps[F, E, A](fa)
}

/**
  * Extension to ApplicativeError in a binary compat way
  */
trait ApplicativeErrorExtension {
  implicit final def catsSyntaxApplicativeErrorExtension[F[_], E](F: ApplicativeError[F, E]):
    ApplicativeErrorExtensionOps[F, E] =
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

}

final class ApplicativeErrorIdOps[E](val e: E) extends AnyVal {
  def raiseError[F[_], A](implicit F: ApplicativeError[F, E]): F[A] =
    F.raiseError(e)
}

final class ApplicativeErrorOps[F[_], E, A](val fa: F[A]) extends AnyVal {
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

  def or(other: F[A])(implicit F: ApplicativeError[F, E]): F[A] =
    F.handleErrorWith(fa)(_ => other)
}
