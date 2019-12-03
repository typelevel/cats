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
   * scala> import cats.implicits._
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
   * scala> import cats.implicits._
   *
   * scala> val sortedSet = SortedSet(12, -2, 3, -5)
   *
   * scala> sortedSet.groupByNes(_ >= 0)
   * res0: SortedMap[Boolean, NonEmptySet[Int]] = Map(false -> TreeSet(-5, -2), true -> TreeSet(3, 12))
   * }}}
   */
  def groupByNes[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptySet[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toNes.fold(SortedMap.empty[B, NonEmptySet[A]])(_.groupBy(f).toSortedMap)
  }
}
