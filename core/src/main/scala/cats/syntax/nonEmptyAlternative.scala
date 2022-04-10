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

trait NonEmptyAlternativeSyntax {
  implicit final def catsSyntaxNonEmptyAlternative[F[_], A](fa: F[A]): NonEmptyAlternativeOps[F, A] =
    new NonEmptyAlternativeOps(fa)
}

final class NonEmptyAlternativeOps[F[_], A] private[syntax] (private val fa: F[A]) extends AnyVal {

  /**
   * @see [[NonEmptyAlternative.prependK]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> List(2, 3, 4).prependK(1)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def prependK(a: A)(implicit F: NonEmptyAlternative[F]): F[A] = F.prependK(a, fa)

  /**
   * @see [[NonEmptyAlternative.appendK]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> List(1, 2, 3).appendK(4)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def appendK(a: A)(implicit F: NonEmptyAlternative[F]): F[A] = F.appendK(fa, a)
}
