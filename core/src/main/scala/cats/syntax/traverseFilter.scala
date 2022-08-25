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

trait TraverseFilterSyntax extends TraverseFilter.ToTraverseFilterOps

private[syntax] trait TraverseFilterSyntaxBinCompat0 {
  implicit def toSequenceFilterOps[F[_], G[_], A](fgoa: F[G[Option[A]]]): SequenceFilterOps[F, G, A] =
    new SequenceFilterOps(fgoa)

  implicit def toTraverseFilterOps[F[_], G[_], A](fa: F[A]): TraverseFilterOps[F, G, A] =
    new TraverseFilterOps(fa)
}

final class SequenceFilterOps[F[_], G[_], A](private val fgoa: F[G[Option[A]]]) extends AnyVal {

  /**
   * {{{
   * scala> import cats.implicits._
   * scala> val a: List[Either[String, Option[Int]]] = List(Right(Some(1)), Right(Some(5)), Right(Some(3)))
   * scala> val b: Either[String, List[Int]] = a.sequenceFilter
   * b: Either[String, List[Int]] = Right(List(1, 5, 3))
   * }}}
   */
  def sequenceFilter(implicit F: TraverseFilter[F], G: Applicative[G]): G[F[A]] = F.sequenceFilter(fgoa)
}

final class TraverseFilterOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {

  def traverseCollect[B](f: PartialFunction[A, G[B]])(implicit
    F: TraverseFilter[F],
    G: Applicative[G]
  ): G[F[B]] =
    F.traverseCollect(fa)(f)
}
