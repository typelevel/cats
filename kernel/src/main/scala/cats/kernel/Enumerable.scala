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
package kernel

import scala.collection.immutable.LazyList
import scala.annotation.tailrec
import scala.{specialized => sp}

/** A typeclass for types which are countable. Formally this means that values
  * can be mapped on to the natural numbers.
  *
  * Because Countable types may be mapped to the natural numbers, being an
  * instance of `Countable` implies having a total ordering, e.g. an `Order`
  * instance. It also implies having a `PartialNext` and `PartialPrevious` as
  * all representations of the countable numbers, or a subset there of, have
  * `PartialNext` and `PartialPrevious`.
  *
  * @note Types which are countable can be both finitely countable and
  *       infinitely countable. The canonical example of this are the natural
  *       numbers themselves. They are countable, but are infinite.
  *
  * @see [[https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Enum.html]]
  * @see [[https://en.wikipedia.org/wiki/Countable_set]]
  */
trait Countable[@sp A] extends PartialNext[A] with PartialPrevious[A]{
  def order: Order[A]
  def fromEnum(a: A): BigInt
  def toEnum(i: BigInt): Option[A]

  /** The fundamental function in the `Countable` class. Given a start position,
    * an offset, and an optional last position, enumerate the values between
    * `first` and `last` (or `MaxValue` or infinity), using a step of `by -
    * first`.
    *
    * {{{
    * scala> Countable[Int].enumFromThenToOpt(1, 3, Some(11)).toList
    * val res0: List[Int] = List(1, 3, 5, 7, 9, 11)
    * }}}
    *
    * All other enum like functions can be expressed in terms of this
    * function.
    */
  def enumFromThenToOpt(first: A, by: A, last: Option[A]): LazyList[A] = {
    val Zero: BigInt = BigInt(0)
    val increment: BigInt = fromEnum(by) - fromEnum(first)

    def loop(i: A): LazyList[A] =
      if (increment > Zero) {
        // forwards
        partialNextByN(i, increment) match {
          case Some(next) =>
            if (last.fold(false)(order.gt(next, _))) {
              LazyList.empty[A]
            } else {
              next #:: loop(next)
            }
          case _ =>
            LazyList.empty[A]
        }
      } else {
          // backwards or zero
          partialPreviousByN(i, increment.abs) match {
            case Some(next) =>
              if (last.fold(false)(order.lt(next, _))) {
                LazyList.empty
              } else {
                next #:: loop(next)
              }
            case _ =>
              LazyList.empty
          }
      }

    last match {
      case Some(last) =>
        order.compare(first, last) match {
          case result if result < Zero =>
            if (increment < Zero) {
              LazyList.empty[A]
            } else {
              first #:: loop(first)
            }
          case result if result > Zero =>
            if (increment > Zero) {
              LazyList.empty[A]
            } else {
              first #:: loop(first)
            }
          case _ =>
            first #:: loop(first)
        }
      case _ =>
        first #:: loop(first)
    }
  }

  /** Given a start position, an offset, and a last position, enumerate the
    * values between `first` and `last`, using a step of `by - first`.
    *
    * {{{
    * scala> Countable[Int].enumFromThenTo(1, 3, 11).toList
    * val res0: List[Int] = List(1, 3, 5, 7, 9, 11)
    * }}}
    */
  def enumFromThenTo(first: A, by: A, last: A): LazyList[A] =
    enumFromThenToOpt(first, by, Some(last))

  /** Given a start position and a last position, enumerate the
    * values between `first` and `last`, using a step of 1.
    *
    * {{{
    * scala> Countable[Int].enumFromTo(1, 5).toList
    * val res0: List[Int] = List(1, 2, 3, 4, 5)
    * }}}
    */
  def enumFromTo(first: A, last: A): LazyList[A] =
    partialNext(first) match {
      case Some(by) =>
        enumFromThenTo(first, by, last)
      case _ =>
        if (order.lteqv(first, last)) {
          LazyList(first)
        } else {
          LazyList.empty
        }
    }

  /** Given a start position and a increment, enumerate the values starting at
    * `first` until `MaxValue` or infinity if the type is unbounded.
    *
    * {{{
    * scala> Countable[Int].enumFromThen(Int.MaxValue - 5, Int.MaxValue - 4).toList
    * val res0: List[Int] = List(2147483642, 2147483643, 2147483644, 2147483645, 2147483646, 2147483647)
    * }}}
    */
  def enumFromThen(first: A, by: A): LazyList[A] =
    enumFromThenToOpt(first, by, None)

  /** Given a start position, enumerate the values starting at `first` by 1,
    * until `MaxValue` or infinity if the type is unbounded.
    *
    * {{{
    * scala> Countable[Int].enumFrom(Int.MaxValue - 5).toList
    * val res0: List[Int] = List(2147483642, 2147483643, 2147483644, 2147483645, 2147483646, 2147483647)
    * }}}
    */
  def enumFrom(first: A): LazyList[A] =
    partialNext(first) match {
      case Some(by) =>
        enumFromThen(first, by)
      case _ =>
        LazyList(first)
    }

  override final def partialOrder: PartialOrder[A] = order
}

object Countable {
  def apply[A](implicit A: Countable[A]): Countable[A] = A
}

/**
 * A typeclass with an operation which returns a member which is
 * greater or `None` than the one supplied.
 */
trait PartialNext[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialNext(a: A): Option[A]

  def partialNextByN(a: A, n: BigInt): Option[A] = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt=  BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): Option[A] =
      if (n <= Zero) {
        Some(acc)
      } else {
        partialNext(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case otherwise =>
            otherwise
        }
      }

    loop(a, n)
  }
}

/**
 * A typeclass with an operation which returns a member which is
 * always greater than the one supplied.
 */
trait Next[@sp A] extends PartialNext[A] {
  def next(a: A): A
  override def partialNext(a: A): Option[A] = Some(next(a))
}

/**
 * A typeclass with an operation which returns a member which is
 * smaller or `None` than the one supplied.
 */
trait PartialPrevious[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialPrevious(a: A): Option[A]

  def partialPreviousByN(a: A, n: BigInt): Option[A] = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt = BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): Option[A] =
      if (n <= Zero) {
        Some(acc)
      } else {
        partialPrevious(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case otherwise =>
            otherwise
        }
      }

    loop(a, n)
  }
}

/**
 * A typeclass with an operation which returns a member which is
 * always smaller than the one supplied.
 */
trait Previous[@sp A] extends PartialPrevious[A] {
  def partialOrder: PartialOrder[A]
  def previous(a: A): A
  override def partialPrevious(a: A): Option[A] = Some(previous(a))
}

/**
 * A typeclass which has both `previous` and `next` operations
 * such that `next . previous == identity`.
 */
trait UnboundedEnumerable[@sp A] extends Next[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait BoundedEnumerable[@sp A] extends PartialPreviousUpperBounded[A] with PartialNextLowerBounded[A] {

  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order

  def cycleNext(a: A): A =
    partialNext(a).getOrElse(minBound)

  def cyclePrevious(a: A): A =
    partialPrevious(a).getOrElse(maxBound)

}

object BoundedEnumerable {
  implicit def catsKernelBoundedEnumerableForUnit: BoundedEnumerable[Unit] =
    cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelBoundedEnumerableForBoolean: BoundedEnumerable[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelBoundedEnumerableForByte: BoundedEnumerable[Byte] =
    cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def catsKernelBoundedEnumerableForInt: BoundedEnumerable[Int] =
    cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelBoundedEnumerableForShort: BoundedEnumerable[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelBoundedEnumerableForLong: BoundedEnumerable[Long] =
    cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelBoundedEnumerableForChar: BoundedEnumerable[Char] =
    cats.kernel.instances.char.catsKernelStdOrderForChar

  @inline def apply[A](implicit e: BoundedEnumerable[A]): BoundedEnumerable[A] = e

  /**
   * Defines a `BoundedEnumerable[A]` from the given enumerable such that
   * all arrows / successor functions switch direction.
   */
  def reverse[@sp A](e: BoundedEnumerable[A]): BoundedEnumerable[A] =
    new BoundedEnumerable[A] {
      override def order: Order[A] = Order.reverse(e.order)

      override def partialNext(a: A): Option[A] = e.partialPrevious(a)
      override def partialPrevious(a: A): Option[A] = e.partialNext(a)

      override def minBound: A = e.maxBound
      override def maxBound: A = e.minBound
    }
}

trait LowerBoundedEnumerable[@sp A] extends PartialNextLowerBounded[A] with Next[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait UpperBoundedEnumerable[@sp A] extends PartialPreviousUpperBounded[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}
