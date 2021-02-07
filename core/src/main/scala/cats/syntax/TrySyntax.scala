package cats
package syntax

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

import scala.util.Try

trait TrySyntax {
  implicit final def catsSyntaxTry[A](t: Try[A]): TryOps[A] = new TryOps(t)
}

final class TryOps[A](private val self: Try[A]) extends AnyVal {

  /**
   * lift the `try` into a `F[_]` with `ApplicativeThrow[F]` instance
   *
   * {{{
   * scala> import cats.implicits._
   * scala> import util.Try
   *
   * scala> val s: Try[Int] = Try(3)
   * scala> s.liftTo[Either[Throwable, *]]
   * res0: Either[Throwable, Int] = Right(3)
   *
   * scala> val f: Try[Int] = Try(throw new Throwable("boo"))
   * scala> f.liftTo[Either[Throwable, *]]
   * res0: Either[Throwable, Int] = Left(java.lang.Throwable: boo)
   * }}}
   */
  def liftTo[F[_]](implicit F: ApplicativeThrow[F]): F[A] =
    F.fromTry(self)

  /**
   * transforms the try to a Validated[Throwable, A] instance
   *
   * {{{
   * scala> import cats.syntax.try_._
   * scala> import cats.data.Validated
   * scala> import util.Try
   *
   * scala> val s: Try[Int] = Try(3)
   * scala> s.toValidated
   * res0: Validated[Throwable, Int] = Valid(3)
   *
   * scala> val f: Try[Int] = Try(throw new Throwable("boo"))
   * scala> f.toValidated
   * res0: Validated[Throwable, Int] = Invalid(java.lang.Throwable: boo)
   * }}}
   */
  def toValidated: Validated[Throwable, A] = self.fold(Invalid(_), Valid(_))

}
