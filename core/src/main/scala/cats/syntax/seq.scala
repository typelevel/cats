package cats.syntax

import cats.Applicative
import cats.Functor
import cats.Order
import cats.Traverse
import cats.data.NonEmptySeq

import scala.collection.immutable.Seq
import scala.collection.immutable.SortedMap

trait SeqSyntax {
  implicit final def catsSyntaxSeqs[A](va: Seq[A]): SeqOps[A] = new SeqOps(va)
}

final class SeqOps[A](private val va: Seq[A]) extends AnyVal {

  /**
   * Returns an `Option` of `NonEmptySeq` from a `Seq`
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val seq1 = Seq(1, 2)
   * scala> seq1.toNeSeq
   * res1: Option[NonEmptySeq[Int]] = Some(NonEmptySeq(1, 2))
   *
   * scala> val seq2 = Seq.empty[Int]
   * scala> seq2.toNeSeq
   * res2: Option[NonEmptySeq[Int]] = None
   * }}}
   */
  def toNeSeq: Option[NonEmptySeq[A]] = NonEmptySeq.fromSeq(va)

  /**
   * Concatenates this `Seq` with a `NonEmptySeq` producing a new `NonEmptySeq`.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> Seq(1, 2, 3).concatNeSeq(NonEmptySeq.of(4, 5, 6))
   * res0: NonEmptySeq[Int] = NonEmptySeq(1, 2, 3, 4, 5, 6)
   * }}}
   */
  def concatNeSeq[AA >: A](neseq: NonEmptySeq[AA]): NonEmptySeq[AA] = neseq.prependSeq(va)

  /**
   * Groups elements inside this `Seq` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   * scala> import scala.collection.immutable.SortedMap
   *
   * scala> val seq = Seq(12, -2, 3, -5)
   * scala> val res = SortedMap(false -> NonEmptySeq.of(-2, -5), true -> NonEmptySeq.of(12, 3))
   * scala> seq.groupByNeSeq(_ >= 0) === res
   * res1: Boolean = true
   * }}}
   */
  def groupByNeSeq[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptySeq[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toNeSeq.fold(SortedMap.empty[B, NonEmptySeq[A]])(_.groupBy(f))
  }

  /**
   * Groups elements inside this `Seq` according to the `Order` of the keys
   * produced by the given mapping monadic function.
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   * scala> import scala.collection.immutable.SortedMap
   *
   * scala> def f(n: Int) = n match { case 0 => None; case n => Some(n > 0) }
   *
   * scala> val seq = Seq(12, -2, 3, -5)
   * scala> val res = Some(SortedMap(false -> NonEmptySeq.of(-2, -5), true -> NonEmptySeq.of(12, 3)))
   * scala> seq.groupByNeSeqA(f) === res
   * res1: Boolean = true
   *
   * scala> // `f(0)` returns `None`
   * scala> (seq :+ 0).groupByNeSeqA(f) === None
   * res2: Boolean = true
   * }}}
   */
  def groupByNeSeqA[F[_], B](
    f: A => F[B]
  )(implicit F: Applicative[F], B: Order[B]): F[SortedMap[B, NonEmptySeq[A]]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    val mapFunctor = Functor[SortedMap[B, *]]
    val nesTraverse = Traverse[NonEmptySeq]

    toNeSeq.fold(F.pure(SortedMap.empty[B, NonEmptySeq[A]])) { nes =>
      F.map(nesTraverse.traverse(nes)(a => F.tupleRight(f(a), a))) { seq =>
        mapFunctor.map(seq.groupBy(_._1))(_.map(_._2))
      }
    }
  }

  /**
   * Produces a `NonEmptySeq` containing cumulative results of applying the
   * operator going left to right.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val seq1 = Seq(1, 2)
   * scala> seq1.scanLeftNeSeq(100)(_ + _)
   * res1: NonEmptySeq[Int] = NonEmptySeq(100, 101, 103)
   *
   * scala> val seq2 = Seq.empty[Int]
   * scala> seq2.scanLeftNeSeq(123)(_ + _)
   * res2: NonEmptySeq[Int] = NonEmptySeq(123)
   * }}}
   */
  def scanLeftNeSeq[B](b: B)(f: (B, A) => B): NonEmptySeq[B] =
    NonEmptySeq.fromSeqUnsafe(va.scanLeft(b)(f))

  /**
   * Produces a `NonEmptySeq` containing cumulative results of applying the
   * operator going right to left.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val seq = Seq(1, 2)
   * scala> seq.scanRightNeSeq(100)(_ + _)
   * res0: NonEmptySeq[Int] = NonEmptySeq(103, 102, 100)
   *
   * scala> val seq2 = Seq.empty[Int]
   * scala> seq2.scanRightNeSeq(123)(_ + _)
   * res1: NonEmptySeq[Int] = NonEmptySeq(123)
   * }}}
   */
  def scanRightNeSeq[B](b: B)(f: (A, B) => B): NonEmptySeq[B] =
    NonEmptySeq.fromSeqUnsafe(va.scanRight(b)(f))
}
