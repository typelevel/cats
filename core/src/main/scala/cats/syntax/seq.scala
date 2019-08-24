package cats.syntax

import cats.Order

import scala.collection.immutable.Seq
import scala.collection.immutable.SortedMap
import cats.data.{NonEmptyChain, NonEmptyList}

import scala.language.implicitConversions

trait SeqSyntax {
  implicit final def catsSyntaxSeq[A](la: Seq[A]): SeqOps[A] = new SeqOps(la)
}

final class SeqOps[A](private val la: Seq[A]) extends AnyVal {

  /**
   * Returns an Option of NonEmptyList from a Seq
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val result1: Seq[Int] = Seq(1, 2)
   * scala> result1.toNel
   * res0: Option[NonEmptyList[Int]] = Some(NonEmptyList(1, 2))
   *
   * scala> val result2: Seq[Int] = Seq.empty[Int]
   * scala> result2.toNel
   * res1: Option[NonEmptyList[Int]] = None
   * }}}
   */
  def toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(la.toList)

  /**
   * Groups elements inside this `Seq` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val seq = Seq(12, -2, 3, -5)
   *
   * scala> seq.groupByNel(_ >= 0)
   * res0: SortedMap[Boolean, NonEmptyList[Int]] = Map(false -> NonEmptyList(-2, -5), true -> NonEmptyList(12, 3))
   * }}}
   */
  def groupByNel[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyList[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toNel.fold(SortedMap.empty[B, NonEmptyList[A]])(_.groupBy(f))
  }

  /**
   * Groups elements inside this `Seq` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val seq = Seq(12, -2, 3, -5)
   *
   * scala> seq.groupByNec(_ >= 0)
   * res0: SortedMap[Boolean, NonEmptyChain[Int]] = Map(false -> Chain(-2, -5), true -> Chain(12, 3))
   * }}}
   */
  def groupByNec[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyChain[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    NonEmptyChain.fromSeq(la).fold(SortedMap.empty[B, NonEmptyChain[A]])(_.groupBy(f).toSortedMap)
  }
}
