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

import cats.data.{Validated, ValidatedNec, ValidatedNel}

trait ValidatedSyntax {
  implicit final def catsSyntaxValidatedId[A](a: A): ValidatedIdSyntax[A] = new ValidatedIdSyntax(a)
}

final class ValidatedIdSyntax[A](private val a: A) extends AnyVal {
  def valid[B]: Validated[B, A] = Validated.Valid(a)
  def validNel[B]: ValidatedNel[B, A] = Validated.Valid(a)
  def invalid[B]: Validated[A, B] = Validated.Invalid(a)
  def invalidNel[B]: ValidatedNel[A, B] = Validated.invalidNel(a)
}

trait ValidatedExtensionSyntax {
  implicit final def catsSyntaxValidatedExtension[E, A](v: Validated[E, A]): ValidatedExtension[E, A] =
    new ValidatedExtension(v)
}

final class ValidatedExtension[E, A](private val self: Validated[E, A]) extends AnyVal {
  def liftTo[F[_]](implicit F: ApplicativeError[F, ? >: E]): F[A] =
    F.fromValidated(self)
}

private[syntax] trait ValidatedSyntaxBincompat0 {
  implicit final def catsSyntaxValidatedIdBinCompat0[A](a: A): ValidatedIdOpsBinCompat0[A] =
    new ValidatedIdOpsBinCompat0(a)
}

final private[syntax] class ValidatedIdOpsBinCompat0[A](private val a: A) extends AnyVal {

  /**
   * Wrap a value to a valid ValidatedNec
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._, cats.data._
   * scala> 1.validNec[String]
   * res0: Validated[NonEmptyChain[String], Int] = Valid(1)
   * }}}
   */
  def validNec[B]: ValidatedNec[B, A] = Validated.Valid(a)

  /**
   * Wrap a value to an invalid ValidatedNec
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._, cats.data._
   * scala> "Err".invalidNec[Int]
   * res0: Validated[NonEmptyChain[String], Int] = Invalid(Chain(Err))
   * }}}
   */
  def invalidNec[B]: ValidatedNec[A, B] = Validated.invalidNec(a)
}
