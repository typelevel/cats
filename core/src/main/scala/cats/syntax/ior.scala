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

package cats.syntax

import cats.data.Ior

trait IorSyntax {
  implicit final def catsSyntaxIorId[A](a: A): IorIdOps[A] = new IorIdOps(a)
}

final class IorIdOps[A](private val a: A) extends AnyVal {

  /**
   * Wrap a value in `Ior.Right`.
   *
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "hello".rightIor[String]
   * res0: Ior[String, String] = Right(hello)
   * }}}
   */
  def rightIor[B]: Ior[B, A] = Ior.right(a)

  /**
   * Wrap a value in `Ior.Left`.
   *
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "error".leftIor[String]
   * res0: Ior[String, String] = Left(error)
   * }}}
   */
  def leftIor[B]: Ior[A, B] = Ior.left(a)
}
