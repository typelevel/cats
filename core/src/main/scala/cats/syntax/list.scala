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

import cats.data.{NonEmptyChain, NonEmptyList}
import scala.collection.immutable.SortedMap

trait ListSyntax {
  implicit final def catsSyntaxList[A](la: List[A]): ListOps[A] = new ListOps(la)
}

final class ListOps[A](private val la: List[A]) extends AnyVal {

  /**
   * Returns an Option of NonEmptyList from a List
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.syntax.all._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.toNel
   * res0: Option[NonEmptyList[Int]] = Some(NonEmptyList(1, 2))
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.toNel
   * res1: Option[NonEmptyList[Int]] = None
   * }}}
   */
  def toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(la)

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.syntax.all._
   *
   * scala> val list = List(12, -2, 3, -5)
   *
   * scala> val expectedResult = SortedMap(false -> NonEmptyList.of(-2, -5), true -> NonEmptyList.of(12, 3))
   *
   * scala> list.groupByNel(_ >= 0) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNel[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyList[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toNel.fold(SortedMap.empty[B, NonEmptyList[A]])(_.groupBy(f))
  }

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given mapping monadic function.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.syntax.all._
   *
   * scala> val list = List(12, -2, 3, -5)
   *
   * scala> val expectedResult = Option(SortedMap(false -> NonEmptyList.of(-2, -5), true -> NonEmptyList.of(12, 3)))
   *
   * scala> list.groupByNelA(num => Option(0).map(num >= _)) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNelA[F[_], B](f: A => F[B])(implicit F: Applicative[F], B: Order[B]): F[SortedMap[B, NonEmptyList[A]]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    val functor = Functor[SortedMap[B, *]]

    toNel.fold(F.pure(SortedMap.empty[B, NonEmptyList[A]]))(nel =>
      F.map(nel.traverse(a => F.tupleLeft(f(a), a)))(list => functor.map(list.groupBy(_._2))(_.map(_._1)))
    )
  }

  /**
   * Produces a `NonEmptyList` containing cumulative results of applying the
   * operator going left to right.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.syntax.all._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.scanLeftNel(100)(_ + _)
   * res0: NonEmptyList[Int] = NonEmptyList(100, 101, 103)
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.scanLeftNel(1)(_ + _)
   * res1: NonEmptyList[Int] = NonEmptyList(1)
   * }}}
   */
  def scanLeftNel[B](b: B)(f: (B, A) => B): NonEmptyList[B] =
    NonEmptyList.fromListUnsafe(la.scanLeft(b)(f))

  /**
   * Produces a `NonEmptyList` containing cumulative results of applying the
   * operator going right to left.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.syntax.all._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.scanRightNel(100)(_ + _)
   * res0: NonEmptyList[Int] = NonEmptyList(103, 102, 100)
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.scanRightNel(1)(_ + _)
   * res1: NonEmptyList[Int] = NonEmptyList(1)
   * }}}
   */
  def scanRightNel[B](b: B)(f: (A, B) => B): NonEmptyList[B] =
    NonEmptyList.fromListUnsafe(la.scanRight(b)(f))
}

private[syntax] trait ListSyntaxBinCompat0 {
  implicit final def catsSyntaxListBinCompat0[A](la: List[A]): ListOpsBinCompat0[A] = new ListOpsBinCompat0(la)
}

final private[syntax] class ListOpsBinCompat0[A](private val la: List[A]) extends AnyVal {

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.syntax.all._
   *
   * scala> val list = List(12, -2, 3, -5)
   *
   * scala> val expectedResult = SortedMap(false -> NonEmptyChain(-2, -5), true -> NonEmptyChain(12, 3))
   *
   * scala> list.groupByNec(_ >= 0) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNec[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyChain[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    NonEmptyChain.fromSeq(la).fold(SortedMap.empty[B, NonEmptyChain[A]])(_.groupBy(f).toSortedMap)
  }
}
