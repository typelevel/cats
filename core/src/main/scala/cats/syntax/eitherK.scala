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

import cats.data.EitherK

trait EitherKSyntax {
  implicit final def catsSyntaxEitherK[F[_], A](a: F[A]): EitherKOps[F, A] = new EitherKOps(a)
}

final class EitherKOps[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Lift an `F[A]` into a `EitherK[F, G, A]` for any type constructor `G[_]`.
   *
   * @see [[rightc]] to swap the order of `F` and `G` in the result type.
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherK
   * scala> import cats.Eval
   * scala> import cats.syntax.all._
   * scala> List(1, 2, 3).leftc[Eval]
   * res0: EitherK[List, Eval, Int] = EitherK(Left(List(1, 2, 3)))
   * }}}
   */
  def leftc[G[_]]: EitherK[F, G, A] = EitherK.leftc(fa)

  /**
   * Lift an `F[A]` into a `EitherK[G, F, A]` for any type constructor `G[_]`.
   *
   * @see [[leftc]] to swap the order of `F` and `G` in the result type.
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherK
   * scala> import cats.Eval
   * scala> import cats.syntax.all._
   * scala> List(1, 2, 3).rightc[Eval]
   * res0: EitherK[Eval, List, Int] = EitherK(Right(List(1, 2, 3)))
   * }}}
   */
  def rightc[G[_]]: EitherK[G, F, A] = EitherK.rightc(fa)
}
