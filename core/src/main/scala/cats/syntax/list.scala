package cats
package syntax

import scala.collection.immutable.SortedMap
import cats.data.NonEmptyList

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
  def groupByNel[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyList[A]] = {
    implicit val ordering = B.toOrdering
    toNel.fold(SortedMap.empty[B, NonEmptyList[A]])(_.groupBy(f))
  }
}
