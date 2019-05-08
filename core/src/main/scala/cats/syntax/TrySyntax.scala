package cats
package syntax

import scala.util.Try

trait TrySyntax {
  implicit final def catsSyntaxTry[A](t: Try[A]): TryOps[A] = new TryOps(t)
}

final class TryOps[A](private val self: Try[A]) extends AnyVal {

  /**
   * lift the `try` into a `F[_]` with `ApplicativeError[F, Throwable]` instance
   *
   * {{{
   * scala> import cats.implicits._
   * scala> import util.Try
   *
   * scala> val s: Try[Int] = Try(3)
   * scala> s.liftTo[Either[Throwable, ?]]
   * res0: Either[Throwable, Int] = Right(3)
   *
   * scala> val f: Try[Int] = Try(throw new Throwable("boo"))
   * scala> f.liftTo[Either[Throwable, ?]]
   * res0: Either[Throwable, Int] = Left(java.lang.Throwable: boo)
   * }}}
   */
  def liftTo[F[_]](implicit F: ApplicativeError[F, Throwable]): F[A] =
    F.fromTry(self)

}
