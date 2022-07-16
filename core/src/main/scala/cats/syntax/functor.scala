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

trait FunctorSyntax extends Functor.ToFunctorOps {
  implicit final def catsSyntaxFunctorTuple2Ops[F[_], A, B](fab: F[(A, B)]): FunctorTuple2Ops[F, A, B] =
    new FunctorTuple2Ops[F, A, B](fab)

  implicit final def catsSyntaxIfF[F[_]](fa: F[Boolean]): IfFOps[F] =
    new IfFOps[F](fa)
}

final class FunctorTuple2Ops[F[_], A, B](private val fab: F[(A, B)]) extends AnyVal {

  /**
   * Lifts `Tuple2#_1` to Functor
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6))._1F == Chain(1, 3, 5)
   * res0: Boolean = true
   * }}}
   */
  def _1F(implicit F: Functor[F]): F[A] = F.map(fab)(_._1)

  /**
   * Lifts `Tuple2#_2` to Functor
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6))._2F == Chain(2, 4, 6)
   * res0: Boolean = true
   * }}}
   */
  def _2F(implicit F: Functor[F]): F[B] = F.map(fab)(_._2)

  /**
   * Lifts `Tuple2#swap` to Functor
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6)).swapF == Chain((2, 1), (4, 3), (6, 5))
   * res0: Boolean = true
   * }}}
   */
  def swapF(implicit F: Functor[F]): F[(B, A)] = F.map(fab)(_.swap)

  /**
   * Un-zips an `F[(A, B)]` consisting of element pairs or Tuple2 into two separate F's tupled.
   *
   * NOTE: Check for effect duplication, possibly memoize before
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6)).unzip == ((Chain(1, 3, 5), Chain(2, 4, 6)))
   * res0: Boolean = true
   * }}}
   */
  def unzip(implicit F: Functor[F]): (F[A], F[B]) = F.unzip(fab)
}

final class IfFOps[F[_]](private val fa: F[Boolean]) extends AnyVal {

  /**
   * Lifts `if` to Functor
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> List(true, false, false).ifF(1, 0)
   * res0: List[Int] = List(1, 0, 0)
   * }}}
   */
  def ifF[B](ifTrue: => B, ifFalse: => B)(implicit F: Functor[F]): F[B] = F.ifF(fa)(ifTrue, ifFalse)
}
