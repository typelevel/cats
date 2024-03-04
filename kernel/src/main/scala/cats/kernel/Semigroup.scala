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

package cats.kernel

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue, Seq, SortedMap, SortedSet}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.{specialized => sp}
import scala.util.{Failure, Success, Try}
import compat.scalaVersionSpecific._

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>

  /**
   * Associative operation which combines two values.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.option._
   *
   * scala> Semigroup[String].combine("Hello ", "World!")
   * res0: String = Hello World!
   *
   * scala> Semigroup[Option[Int]].combine(None, Some(1))
   * res1: Option[Int] = Some(1)
   * }}}
   */
  def combine(x: A, y: A): A

  /**
   * Return `a` combined with itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[Int].combineN(1, 10)
   * res0: Int = 10
   *
   * scala> Semigroup[String].combineN("ha", 3)
   * res1: String = hahaha
   * }}}
   */
  def combineN(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combining for semigroups must have n > 0")
    else repeatedCombineN(a, n)

  /**
   * Return `a` combined with itself more than once.
   */
  protected[this] def repeatedCombineN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) combine(b, extra)
      else {
        val x = if ((k & 1) == 1) combine(b, extra) else extra
        loop(combine(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[String].combineAllOption(List("One ", "Two ", "Three"))
   * res0: Option[String] = Some(One Two Three)
   *
   * scala> Semigroup[String].combineAllOption(List.empty)
   * res1: Option[String] = None
   * }}}
   */
  def combineAllOption(as: IterableOnce[A]): Option[A] =
    as.reduceOption(combine)

  /**
   * return a semigroup that reverses the order
   * so combine(a, b) == reverse.combine(b, a)
   */
  def reverse: Semigroup[A] =
    new Semigroup[A] {
      def combine(a: A, b: A): A = self.combine(b, a)
      // a + a + a + ... is the same when reversed
      override def combineN(a: A, n: Int): A = self.combineN(a, n)
      override def reverse = self
    }

  /**
   * Between each pair of elements insert middle
   * This name matches the term used in Foldable and Reducible and a similar Haskell function.
   */
  def intercalate(middle: A): Semigroup[A] =
    (a, b) => self.combine(a, self.combine(middle, b))
}

abstract class SemigroupFunctions[S[T] <: Semigroup[T]] {
  def combine[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.combine(x, y)

  def maybeCombine[@sp(Int, Long, Float, Double) A](ox: Option[A], y: A)(implicit ev: S[A]): A =
    ox match {
      case Some(x) => ev.combine(x, y)
      case None    => y
    }

  def maybeCombine[@sp(Int, Long, Float, Double) A](x: A, oy: Option[A])(implicit ev: S[A]): A =
    oy match {
      case Some(y) => ev.combine(x, y)
      case None    => x
    }

  def isCommutative[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[CommutativeSemigroup[?]]

  def isIdempotent[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[Band[?]]

  def combineN[@sp(Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: S[A]): A =
    ev.combineN(a, n)

  def combineAllOption[A](as: IterableOnce[A])(implicit ev: S[A]): Option[A] =
    ev.combineAllOption(as)
}

object Semigroup
    extends SemigroupFunctions[Semigroup]
    with ScalaVersionSpecificMonoidInstances
    with instances.TupleCommutativeGroupInstances
    with GroupInstances {

  /**
   * Access an implicit `Semigroup[A]`.
   */
  @inline final def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev

  /**
   * Create a `Semigroup` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): Semigroup[A] = cmb(_, _)

  /**
   * Create a `Semigroup` instance that always returns the lefthand side.
   */
  @inline def first[A]: Semigroup[A] = (x, _) => x

  /**
   * Create a `Semigroup` instance that always returns the righthand side.
   */
  @inline def last[A]: Semigroup[A] = (_, y) => y

  implicit def catsKernelBoundedSemilatticeForBitSet: BoundedSemilattice[BitSet] =
    cats.kernel.instances.bitSet.catsKernelStdSemilatticeForBitSet
  implicit def catsKernelInstancesForUnit: BoundedSemilattice[Unit] with CommutativeGroup[Unit] =
    cats.kernel.instances.unit.catsKernelStdAlgebraForUnit
  implicit def catsKernelCommutativeGroupForByte: CommutativeGroup[Byte] =
    cats.kernel.instances.byte.catsKernelStdGroupForByte
  implicit def catsKernelCommutativeGroupForShort: CommutativeGroup[Short] =
    cats.kernel.instances.short.catsKernelStdGroupForShort
  implicit def catsKernelCommutativeGroupForInt: CommutativeGroup[Int] =
    cats.kernel.instances.int.catsKernelStdGroupForInt
  implicit def catsKernelCommutativeGroupForLong: CommutativeGroup[Long] =
    cats.kernel.instances.long.catsKernelStdGroupForLong
  implicit def catsKernelCommutativeGroupForBigInt: CommutativeGroup[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdGroupForBigInt
  implicit def catsKernelCommutativeGroupForBigDecimal: CommutativeGroup[BigDecimal] =
    cats.kernel.instances.bigDecimal.catsKernelStdGroupForBigDecimal
  implicit def catsKernelCommutativeGroupForDuration: CommutativeGroup[Duration] =
    cats.kernel.instances.duration.catsKernelStdGroupForDuration
  implicit def catsKernelCommutativeGroupForFiniteDuration: CommutativeGroup[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdGroupForFiniteDuration
  implicit def catsKernelCommutativeGroupForDouble: CommutativeGroup[Double] =
    cats.kernel.instances.double.catsKernelStdGroupForDouble
  implicit def catsKernelCommutativeGroupForFloat: CommutativeGroup[Float] =
    cats.kernel.instances.float.catsKernelStdGroupForFloat

  implicit def catsKernelMonoidForString: Monoid[String] = cats.kernel.instances.string.catsKernelStdMonoidForString

  implicit def catsKernelMonoidForList[A]: Monoid[List[A]] = cats.kernel.instances.list.catsKernelStdMonoidForList[A]
  implicit def catsKernelMonoidForVector[A]: Monoid[Vector[A]] =
    cats.kernel.instances.vector.catsKernelStdMonoidForVector[A]
  implicit def catsKernelMonoidForQueue[A]: Monoid[Queue[A]] =
    cats.kernel.instances.queue.catsKernelStdMonoidForQueue[A]

  implicit def catsKernelCommutativeGroupForFunction0[A: CommutativeGroup]: CommutativeGroup[() => A] =
    cats.kernel.instances.function.catsKernelCommutativeGroupForFunction0[A]
  implicit def catsKernelCommutativeGroupForFunction1[A, B: CommutativeGroup]: CommutativeGroup[A => B] =
    cats.kernel.instances.function.catsKernelCommutativeGroupForFunction1[A, B]

  implicit def catsKernelBoundedSemilatticeForSet[A]: BoundedSemilattice[Set[A]] =
    cats.kernel.instances.set.catsKernelStdSemilatticeForSet[A]
  implicit def catsKernelBoundedSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdBoundedSemilatticeForSortedSet[A]

  implicit def catsKernelCommutativeMonoidForMap[K, V: CommutativeSemigroup]: CommutativeMonoid[Map[K, V]] =
    cats.kernel.instances.map.catsKernelStdCommutativeMonoidForMap[K, V]
  implicit def catsKernelCommutativeSemigroupForSortedMap[K, V: CommutativeSemigroup]
    : CommutativeSemigroup[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdCommutativeSemigroupForSortedMap[K, V]
  implicit def catsKernelCommutativeMonoidForSortedMap[K: Order, V: CommutativeSemigroup]
    : CommutativeMonoid[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdCommutativeMonoidForSortedMap[K, V]
}

private[kernel] trait GroupInstances extends BoundedSemilatticeInstances {
  implicit def catsKernelGroupForFunction0[A: Group]: Group[() => A] =
    cats.kernel.instances.function.catsKernelGroupForFunction0[A]
  implicit def catsKernelGroupForFunction1[A, B: Group]: Group[A => B] =
    cats.kernel.instances.function.catsKernelGroupForFunction1[A, B]
}

private[kernel] trait BoundedSemilatticeInstances extends SemilatticeInstances {
  implicit def catsKernelBoundedSemilatticeForFunction0[A: BoundedSemilattice]: BoundedSemilattice[() => A] =
    cats.kernel.instances.function.catsKernelBoundedSemilatticeForFunction0[A]
  implicit def catsKernelBoundedSemilatticeForFunction1[A, B: BoundedSemilattice]: BoundedSemilattice[A => B] =
    cats.kernel.instances.function.catsKernelBoundedSemilatticeForFunction1[A, B]
}

private[kernel] trait SemilatticeInstances extends CommutativeMonoidInstances {
  implicit def catsKernelSemilatticeForFunction0[A: Semilattice]: Semilattice[() => A] =
    cats.kernel.instances.function.catsKernelSemilatticeForFunction0[A]
  implicit def catsKernelSemilatticeForFunction1[A, B: Semilattice]: Semilattice[A => B] =
    cats.kernel.instances.function.catsKernelSemilatticeForFunction1[A, B]
}

private[kernel] trait CommutativeMonoidInstances extends MonoidInstances {
  implicit def catsKernelCommutativeMonoidForFunction0[A: CommutativeMonoid]: CommutativeMonoid[() => A] =
    cats.kernel.instances.function.catsKernelCommutativeMonoidForFunction0[A]
  implicit def catsKernelCommutativeMonoidForFunction1[A, B: CommutativeMonoid]: CommutativeMonoid[A => B] =
    cats.kernel.instances.function.catsKernelCommutativeMonoidForFunction1[A, B]
  implicit def catsKernelCommutativeMonoidForOption[A: CommutativeSemigroup]: CommutativeMonoid[Option[A]] =
    cats.kernel.instances.option.catsKernelStdCommutativeMonoidForOption[A]
}

private[kernel] trait MonoidInstances extends BandInstances {
  implicit def catsKernelMonoidForFunction0[A: Monoid]: Monoid[() => A] =
    cats.kernel.instances.function.catsKernelMonoidForFunction0[A]
  implicit def catsKernelMonoidForFunction1[A, B: Monoid]: Monoid[A => B] =
    cats.kernel.instances.function.catsKernelMonoidForFunction1[A, B]
  implicit def catsKernelMonoidForMap[K, V: Semigroup]: Monoid[Map[K, V]] =
    cats.kernel.instances.map.catsKernelStdMonoidForMap[K, V]
  implicit def catsKernelSemigroupForSortedMap[K, V: Semigroup]: Semigroup[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdSemigroupForSortedMap[K, V]
  implicit def catsKernelMonoidForSortedMap[K: Order, V: Semigroup]: Monoid[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdMonoidForSortedMap[K, V]
  implicit def catsKernelMonoidForEither[A, B: Monoid]: Monoid[Either[A, B]] =
    cats.kernel.instances.either.catsDataMonoidForEither[A, B]
  implicit def catsKernelMonoidForTry[A: Monoid]: Monoid[Try[A]] =
    new TryMonoid[A](Monoid[A])

  /**
   * @deprecated
   *   Any non-pure use of [[scala.concurrent.Future Future]] with Cats is error prone
   *   (particularly the semantics of [[cats.Traverse#traverse traverse]] with regard to execution order are unspecified).
   *   We recommend using [[https://typelevel.org/cats-effect/ Cats Effect `IO`]] as a replacement for ''every'' use case of [[scala.concurrent.Future Future]].
   *   However, at this time there are no plans to remove these instances from Cats.
   *
   * @see [[https://github.com/typelevel/cats/issues/4176 Changes in Future traverse behavior between 2.6 and 2.7]]
   */
  implicit def catsKernelMonoidForFuture[A](implicit A: Monoid[A], ec: ExecutionContext): Monoid[Future[A]] =
    new FutureMonoid[A](A, ec)

  implicit def catsKernelMonoidForOption[A: Semigroup]: Monoid[Option[A]] =
    cats.kernel.instances.option.catsKernelStdMonoidForOption[A]
  implicit def catsKernelMonoidForSeq[A]: Monoid[Seq[A]] =
    cats.kernel.instances.seq.catsKernelStdMonoidForSeq[A]
}

private[kernel] trait BandInstances extends CommutativeSemigroupInstances {
  implicit def catsKernelBandForFunction0[A: Band]: Band[() => A] =
    cats.kernel.instances.function.catsKernelBandForFunction0[A]
  implicit def catsKernelBandForFunction1[A, B: Band]: Band[A => B] =
    cats.kernel.instances.function.catsKernelBandForFunction1[A, B]
}

private[kernel] trait CommutativeSemigroupInstances extends SemigroupInstances {
  implicit def catsKernelCommutativeSemigroupForFunction0[A: CommutativeSemigroup]: CommutativeSemigroup[() => A] =
    cats.kernel.instances.function.catsKernelCommutativeSemigroupForFunction0[A]
  implicit def catsKernelCommutativeSemigroupForFunction1[A, B: CommutativeSemigroup]: CommutativeSemigroup[A => B] =
    cats.kernel.instances.function.catsKernelCommutativeSemigroupForFunction1[A, B]
}

private[kernel] trait SemigroupInstances {
  implicit def catsKernelSemigroupForFunction0[A: Semigroup]: Semigroup[() => A] =
    cats.kernel.instances.function.catsKernelSemigroupForFunction0[A]
  implicit def catsKernelSemigroupForFunction1[A, B: Semigroup]: Semigroup[A => B] =
    cats.kernel.instances.function.catsKernelSemigroupForFunction1[A, B]
  implicit def catsKernelSemigroupForEither[A, B: Semigroup]: Semigroup[Either[A, B]] =
    cats.kernel.instances.either.catsDataSemigroupForEither[A, B]
  implicit def catsKernelSemigroupForTry[A: Semigroup]: Semigroup[Try[A]] =
    new TrySemigroup[A](Semigroup[A])

  /**
   * @deprecated
   *   Any non-pure use of [[scala.concurrent.Future Future]] with Cats is error prone
   *   (particularly the semantics of [[cats.Traverse#traverse traverse]] with regard to execution order are unspecified).
   *   We recommend using [[https://typelevel.org/cats-effect/ Cats Effect `IO`]] as a replacement for ''every'' use case of [[scala.concurrent.Future Future]].
   *
   * @see [[https://github.com/typelevel/cats/issues/4176 Changes in Future traverse behavior between 2.6 and 2.7]]
   */
  implicit def catsKernelSemigroupForFuture[A](implicit A: Semigroup[A], ec: ExecutionContext): Semigroup[Future[A]] =
    new FutureSemigroup[A](A, ec)
}

private class TryMonoid[A](A: Monoid[A]) extends TrySemigroup[A](A) with Monoid[Try[A]] {
  def empty: Try[A] = Success(A.empty)
}

private class TrySemigroup[A](A: Semigroup[A]) extends Semigroup[Try[A]] {
  def combine(x: Try[A], y: Try[A]): Try[A] =
    (x, y) match {
      case (Success(xv), Success(yv)) => Success(A.combine(xv, yv))
      case (f @ Failure(_), _)        => f
      case (_, f)                     => f
    }
}

private class FutureMonoid[A](A: Monoid[A], ec: ExecutionContext)
    extends FutureSemigroup[A](A, ec)
    with Monoid[Future[A]] {
  def empty: Future[A] = Future.successful(A.empty)
}

private class FutureSemigroup[A](A: Semigroup[A], ec: ExecutionContext) extends Semigroup[Future[A]] {
  def combine(x: Future[A], y: Future[A]): Future[A] = x.flatMap(xv => y.map(A.combine(xv, _))(ec))(ec)
}
