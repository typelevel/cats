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
   * scala> import cats.syntax.all._
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
