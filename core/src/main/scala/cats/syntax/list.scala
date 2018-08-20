package cats
package syntax

import scala.collection.immutable.SortedMap
import cats.data.{NonEmptyChain, NonEmptyList}

trait ListSyntax {
  implicit final def catsSyntaxList[A](la: List[A]): ListOps[A] = new ListOps(la)
}

final class ListOps[A](val la: List[A]) extends AnyVal {

  /**
    * Returns an Option of NonEmptyList from a List
    *
    * Example:
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> import cats.implicits._
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
    * scala> import cats.implicits._
    *
    * scala> val list = List(12, -2, 3, -5)
    *
    * scala> list.groupByNel(_ >= 0)
    * res0: SortedMap[Boolean, NonEmptyList[Int]] = Map(false -> NonEmptyList(-2, -5), true -> NonEmptyList(12, 3))
    * }}}
    */
  def groupByNel[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyList[A]] = {
    implicit val ordering = B.toOrdering
    toNel.fold(SortedMap.empty[B, NonEmptyList[A]])(_.groupBy(f))
  }
}

trait ListSyntaxBinCompat0 {
  implicit final def catsSyntaxListBinCompat0[A](la: List[A]): ListOpsBinCompat0[A] = new ListOpsBinCompat0(la)
}

final class ListOpsBinCompat0[A](val la: List[A]) extends AnyVal {

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   *
   * scala> val list = List(12, -2, 3, -5)
   *
   * scala> list.groupByNec(_ >= 0)
   * res0: SortedMap[Boolean, NonEmptyChain[Int]] = Map(false -> Chain(-2, -5), true -> Chain(12, 3))
   * }}}
   */
  def groupByNec[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyChain[A]] = {
    implicit val ordering = B.toOrdering
    NonEmptyChain.fromSeq(la).fold(SortedMap.empty[B, NonEmptyChain[A]])(_.groupBy(f).toSortedMap)
  }
}
