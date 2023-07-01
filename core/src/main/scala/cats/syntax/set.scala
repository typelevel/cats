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

import scala.collection.immutable.{SortedMap, SortedSet}
import cats.data.NonEmptySet
import cats.Order

trait SetSyntax {
  implicit final def catsSyntaxSet[A](se: SortedSet[A]): SetOps[A] = new SetOps(se)
}

final class SetOps[A](private val se: SortedSet[A]) extends AnyVal {

  /**
   * Returns an Option of NonEmptySet from a SortedSet
   *
   * Example:
   * {{{
   * scala> import scala.collection.immutable.SortedSet
   * scala> import cats.data.NonEmptySet
   * scala> import cats.syntax.all._
   *
   * scala> val result1: SortedSet[Int] = SortedSet(1, 2)
   * scala> result1.toNes
   * res0: Option[NonEmptySet[Int]] = Some(TreeSet(1, 2))
   *
   * scala> val result2: SortedSet[Int] = SortedSet.empty[Int]
   * scala> result2.toNes
   * res1: Option[NonEmptySet[Int]] = None
   * }}}
   */
  def toNes: Option[NonEmptySet[A]] = NonEmptySet.fromSet(se)

  /**
   * Groups elements inside this `SortedSet` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import scala.collection.immutable.{SortedMap, SortedSet}
   * scala> import cats.syntax.all._
   *
   * scala> val sortedSet = SortedSet(12, -2, 3, -5)
   *
   * scala> val expectedResult = SortedMap(false -> NonEmptySet.of(-5, -2), true -> NonEmptySet.of(3, 12))
   *
   * scala> sortedSet.groupByNes(_ >= 0) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNes[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptySet[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toNes.fold(SortedMap.empty[B, NonEmptySet[A]])(_.groupBy(f).toSortedMap)
  }
}
