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

import scala.annotation.tailrec
import scala.{specialized => sp}
import cats.kernel.{ScalaVersionSpecificLazyListCompat => LazyListLike}

/** A typeclass for types which are countable. Formally this means that values
  * can be mapped on to the natural numbers.
  *
  * Because Enumerable types may be mapped to the natural numbers, being an
  * instance of `Enumerable` implies having a total ordering, e.g. an `Order`
  * instance. It also implies having a `PartialNext` and `PartialPrevious` as
  * all representations of the countable numbers, or a subset there of, have
  * `PartialNext` and `PartialPrevious`.
  *
  * Instances of `Enumerable` require that the if `Order[A].comparison(x, y)
  * <-> Order[BigInt](fromEnum(x), fromEnum(y))`. The ordering of a value of
  * `A` corresponds to the ordering of the `BigInt` mapping of that `A`. This
  * is because all of the useful functions defined by `Enumerable` require
  * this correspondence to provide their utility. For example, they use the
  * difference between two elements of `A` mapped onto `BigInt` to understand
  * the step between enumerated values.
  *
  * @note Types which are countable can be both finitely countable and
  *       infinitely countable. The canonical example of this are the natural
  *       numbers themselves. They are countable, but are infinite.
  *
  * @see [[https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Enum.html]]
  * @see [[https://en.wikipedia.org/wiki/Countable_set]]
  */
trait Enumerable[@sp A] extends PartialNext[A] with PartialPrevious[A]{
  def order: Order[A]
  override final def partialOrder: PartialOrder[A] = order

  /** Convert a value of `A` into its corresponding integer representation.
    */
  def fromEnum(a: A): BigInt

  /** Attempt to convert a `BigInt` into its corresponding representation in
    * this enumeration, yielding `None` if the given `BigInt` is outside the
    * domain of this enumeration.
    */
  def toEnumOpt(i: BigInt): Option[A]

  /** The fundamental function in the `Enumerable` class. Given a `first`
    * element, a second element, and an optional `last` element, enumerate the
    * values between `first` and `last` (or `MaxValue` or infinity), the step
    * between the first and second element as the step between all elements.
    *
    * {{{
    * scala> Enumerable[Int].enumFromThenToOpt(1, 3, Some(11)).toList
    * val res0: List[Int] = List(1, 3, 5, 7, 9, 11)
    * }}}
    *
    * @note If the last element is defined, and the second element is less
    *       than the last element, then the last element will not be part of
    *       the result.
    *
    * @note The last element will only be included in the enumerated result if
    *       it aligns with the step. For example, `enumFromThenToOpt(1, 3,
    *       6).toList`, would be `List(1, 3, 5)`.
    *
    * {{{
    * scala> Enumerable[Int].enumFromThenToOpt(1, 2, Some(1)).toList
    * val res0: List[Int] = List(1)
    * }}}
    *
    * All other enum like functions can be expressed in terms of this
    * function.
    */
  def enumFromThenToOpt(first: A, second: A, last: Option[A]): LazyListLike.T[A] = {
    val Zero: BigInt = BigInt(0)
    val increment: BigInt = fromEnum(second) - fromEnum(first)

    def loop(i: A): LazyListLike.T[A] =
      if (increment > Zero) {
        // forwards
        partialNextByN(i, increment) match {
          case Some(next) =>
            if (last.fold(false)(order.gt(next, _))) {
              LazyListLike.empty[A]
            } else {
              next #:: loop(next)
            }
          case _ =>
            LazyListLike.empty[A]
        }
      } else {
          // backwards or zero
          partialPreviousByN(i, increment.abs) match {
            case Some(next) =>
              if (last.fold(false)(order.lt(next, _))) {
                LazyListLike.empty
              } else {
                next #:: loop(next)
              }
            case _ =>
              LazyListLike.empty
          }
      }

    last match {
      case Some(last) =>
        order.compare(first, last) match {
          case result if result < Zero =>
            if (increment < Zero) {
              LazyListLike.empty[A]
            } else {
              first #:: loop(first)
            }
          case result if result > Zero =>
            if (increment > Zero) {
              LazyListLike.empty[A]
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

  def enumFromByToOpt(first: A, step: BigInt, last: Option[A]): LazyListLike.T[A] =
    toEnumOpt(step).fold(
      LazyListLike.empty[A]
    )(second =>
      enumFromThenToOpt(first, second, last)
    )

  /** Given a `first` element, a second element, and a last, enumerate the
    * values between `first` and `last`, using a step between the `first` and
    * the `second` element as step between all elements.
    *
    * {{{
    * scala> Enumerable[Int].enumFromThenTo(1, 3, 11).toList
    * val res0: List[Int] = List(1, 3, 5, 7, 9, 11)
    * }}}
    *
    * @see [[#enumFromThenToOpt]]
    */
  def enumFromThenTo(first: A, second: A, last: A): LazyListLike.T[A] =
    enumFromThenToOpt(first, second, Some(last))

  def enumFromByTo(first: A, step: BigInt, last: A): LazyListLike.T[A] =
    enumFromByToOpt(first, step, Some(last))

  /** Given a first element and a last element, enumerate the
    * values between `first` and `last`, using a step of 1.
    *
    * {{{
    * scala> Enumerable[Int].enumFromTo(1, 5).toList
    * val res0: List[Int] = List(1, 2, 3, 4, 5)
    * }}}
    */
  def enumFromTo(first: A, last: A): LazyListLike.T[A] =
    partialNext(first) match {
      case Some(by) =>
        enumFromThenTo(first, by, last)
      case _ =>
        if (order.lteqv(first, last)) {
          LazyListLike(first)
        } else {
          LazyListLike.empty
        }
    }

  /** Given a first element and second element, enumerate all values in the
    * domain starting at first using the step between first and second as the
    * step between all elements. If the domain is infinite, e.g. natural
    * numbers or integers, then this will be an infinite result.
    *
    * {{{
    * scala> Enumerable[Int].enumFromThen(Int.MaxValue - 5, Int.MaxValue - 4).toList
    * val res0: List[Int] = List(2147483642, 2147483643, 2147483644, 2147483645, 2147483646, 2147483647)
    * }}}
    */
  def enumFromThen(first: A, second: A): LazyListLike.T[A] =
    enumFromThenToOpt(first, second, None)

  def enumFromBy(first: A, by: BigInt): LazyListLike.T[A] =
    enumFromByToOpt(first, by, None)

  /** Given a first element, enumerate all values in the domain starting at
    * first using a step of difference between the next element and the first
    * element. If the domain is infinite, e.g. natural numbers or integers,
    * then this will be an infinite result.
    *
    * {{{
    * scala> Enumerable[Int].enumFrom(Int.MaxValue - 5).toList
    * val res0: List[Int] = List(2147483642, 2147483643, 2147483644, 2147483645, 2147483646, 2147483647)
    * }}}
    */
  def enumFrom(first: A): LazyListLike.T[A] =
    partialNext(first) match {
      case Some(by) =>
        enumFromThen(first, by)
      case _ =>
        LazyListLike(first)
    }

  /** All members of this enumerable starting at the min bound and continuing
    * upward.
    *
    * @note If the type has no max bound, then this will be an infinite
    *       list.
    */
  def enumFromMin(implicit A: LowerBounded[A]): LazyListLike.T[A] =
    partialNext(A.minBound).fold(
      LazyListLike(A.minBound)
    )(next =>
      enumFromThen(A.minBound, next)
    )

  /** All members of this enumerable starting at the max bound and continuing
    * downward.
    *
    * @note If the type has no min bound, then this will be an infinite
    *       list.
    */
  def enumFromMax(implicit A: UpperBounded[A]): LazyListLike.T[A] =
    partialPrevious(A.maxBound).fold(
      LazyListLike(A.maxBound)
    )(prev =>
      enumFromThen(A.maxBound, prev)
    )
}

object Enumerable {
  def apply[A](implicit A: Enumerable[A]): Enumerable[A] = A

  def reverse[A](A: Enumerable[A]): Enumerable[A] =
    new Enumerable[A] {
      override def fromEnum(a: A): BigInt =
        -A.fromEnum(a)

      override def toEnumOpt(i: BigInt): Option[A] =
        A.toEnumOpt(-i)

      override val order: Order[A] =
        Order.reverse(A.order)

      override def partialNext(a: A): Option[A] =
        A.partialPrevious(a)

      override def partialPrevious(a: A): Option[A] =
        A.partialNext(a)
    }

  implicit def catsKernelEnumerableForBigInt: Enumerable[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
  implicit def catsKernelEnumerableForInt: Enumerable[Int] =
    cats.kernel.instances.int.catsKernelStdBoundableEnumerableForInt
  implicit def catsKernelEnumerableForUnit: Enumerable[Unit] =
    cats.kernel.instances.unit.catsKernelStdBoundableEnumerableForUnit
  implicit def catsKernelEnumerableForBoolean: Enumerable[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdBoundableEnumerableForBoolean
  implicit def catsKernelEnumerableForByte: Enumerable[Byte] =
    cats.kernel.instances.byte.catsKernelStdBoundableEnumerableForByte
  implicit def catsKernelEnumerableForShort: Enumerable[Short] =
    cats.kernel.instances.short.catsKernelStdBoundableEnumerableForShort
  implicit def catsKernelEnumerableForLong: Enumerable[Long] =
    cats.kernel.instances.long.catsKernelStdBoundableEnumerableForLong
  implicit def catsKernelEnumerableForChar: Enumerable[Char] =
    cats.kernel.instances.char.catsKernelStdBoundableEnumerableForChar
}

/**
 * A typeclass with an operation which returns a member which is
 * greater or `None` than the one supplied.
 */
trait PartialNext[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialNext(a: A): Option[A]

  /** As [[#partialNext]], but rather than getting the next element, it gets the
    * Nth next element.
    */
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

  def nextOrMinByN(a: A, n: BigInt)(implicit A: LowerBounded[A]): A = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt=  BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): A =
      if (n <= Zero) {
        acc
      } else {
        partialNext(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case _ =>
            loop(A.minBound, n - One)
        }
      }

    loop(a, n)
  }

  /** Get the next value if defined, otherwise get the minBound. */
  def nextOrMin(a: A)(implicit A: LowerBounded[A]): A =
    nextOrMinByN(a, BigInt(1))

  /** Create an infinite cycling lazy list starting from the given value with
    * each subsequent value N steps ahead of the last. When there is no next
    * value, e.g. `partialNext` returns `None`, restart the cycle from the
    * minBound.
    */
  def cycleForwardFromByN(start: A, n: BigInt)(implicit A: LowerBounded[A]): LazyListLike.T[A] =
    start #:: cycleForwardFromBy(nextOrMinByN(start, n), n)

  /** Create an infinite cycling lazy list starting from the given value. When
    * there is no next value, e.g. `partialNext` returns `None`, restart the
    * cycle from the minBound.
    *
    * @note This will only enumerate all the elements of the set if this type
    *       is a [[BoundableEnumerable]]. If the type is a
    *       [[BoundlessEnumerable]] then the cycle will never restart. If the
    *       type is neither, then it is possible the first cycle will
    *       enumerate elements that are not in following cycles. That is, if
    *       `nextOrMin` starting from `minBound` yields `None` before reaching
    *       `start`, `start` and elements following `start` will only be in
    *       the first cycle. This is not possible for [[BoundableEnumerable]]
    *       or [[BoundlessEnumerable]] types, but may be possible for types
    *       which only have a [[PartialNext]] instance.
    */
  def cycleForwardFrom(start: A)(implicit A: LowerBounded[A]): LazyListLike.T[A] =
    cycleForwardFromBy(start, BigInt(1))

  /** As [[#cycleForwardFromByN]], but uses the minBound as the start value. */
  def cycleForwardByN(n: BigInt)(implicit A: LowerBounded[A]): LazyListLike.T[A] =
    cycleForwardFromByN(A.minBound, n)

  /** As [[#cycleForwardFrom]], but uses the minBound as the start value.
    *
    * Because this cycle starts at the minBound, each cycle will have the same
    * elements in it. However, as with [[#cycleForwardFrom]], each cycle will
    * only contain all elements of `A` if `A` is either a
    * [[BoundableEnumerable]] or [[BoundlessEnumerable]] type.
    */
  def cycleForward(implicit A: LowerBounded[A]): LazyListLike.T[A] =
    cycleForwardFrom(A.minBound)
}

/**
 * A typeclass with an operation which returns a member which is
 * always greater than the one supplied.
 */
trait Next[@sp A] extends PartialNext[A] {
  def next(a: A): A

  override final def nextOrMin(a: A)(implicit A: LowerBounded[A]): A =
    next(a)

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

  def previousOrMax(a: A)(implicit A: UpperBounded[A]): A =
    partialPrevious(a).getOrElse(A.maxBound)
}

/**
 * A typeclass with an operation which returns a member which is
 * always smaller than the one supplied.
 */
trait Previous[@sp A] extends PartialPrevious[A] {
  def partialOrder: PartialOrder[A]
  def previous(a: A): A

  override final def previousOrMax(a: A)(implicit A: UpperBounded[A]): A =
    previous(a)

  override def partialPrevious(a: A): Option[A] = Some(previous(a))
}

/**
 * A typeclass which has both `previous` and `next` operations
 * such that `next . previous == identity`.
 */
// TODO: Not sure what to do about UnboundedEnumerable. It should extend
// Enumerable, but we can't do that without breaking
// bincompat. BoundlessEnumerable could extend UnboundedEnumerable, but that
// seems silly...
trait UnboundedEnumerable[@sp A] extends Next[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait BoundlessEnumerable[@sp A] extends Enumerable[A] with Next[A] with Previous[A] {
  def toEnum(i: BigInt): A

  override final def toEnumOpt(i: BigInt): Option[A] = Some(toEnum(i))
}

object BoundlessEnumerable {
  def apply[A](implicit A: BoundlessEnumerable[A]): BoundlessEnumerable[A] = A

  implicit def catsKernelBoundlessEnumerableForInt: BoundlessEnumerable[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
}

@deprecated(message = "Please use BoundableEnumerable instead.", since = "2.10.0")
trait BoundedEnumerable[@sp A] extends PartialPreviousUpperBounded[A] with PartialNextLowerBounded[A] {

  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order

  @deprecated(message = "Please use nextOrMin.", since = "2.10.0")
  def cycleNext(a: A): A =
    nextOrMin(a)(this)

  @deprecated(message = "Please use previousOrMax instead.", since = "2.10.0")
  def cyclePrevious(a: A): A =
    previousOrMax(a)(this)
}

@deprecated(message = "Please use BoundableEnumerable instead.", since = "2.10.0")
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

trait BoundableEnumerable[@sp A] extends Enumerable[A] with UpperBounded[A] with LowerBounded[A] {

  // If [[#fromEnum]] is defined in such a way that the elements of this set
  // map to elements of the set of integers ''in the same order'', then this
  // is can be defined as `fromEnum(maxBound) - fromEnum(maxBound) +
  // BigInt(1)`, if and only if for all members of this set, `fromEnum(x) <
  // fromEnum(y) => x < y`. Or in other words, this set can be ordered using
  // `Order[BigInt]` and this set's mapping to `BigInt`. This property is
  // likely to hold for most instances of this type, though it is not required
  // to hold for the type to be a lawful instance. Indeed, an `Inverse[A]`
  // type, which flips the ordering of the underlying type would not hold to
  // this property for the simple derived implementation.
  /** The number of elements in the set defined by this enumerable. */
  def size: BigInt
}

object BoundableEnumerable {
  def apply[A](implicit A: BoundableEnumerable[A]): BoundableEnumerable[A] = A
}

@deprecated(message = "Please use Enumerable instead.", since = "2.10.0")
trait LowerBoundedEnumerable[@sp A] extends PartialNextLowerBounded[A] with Next[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

@deprecated(message = "Please use Enumerable instead.", since = "2.10.0")
trait UpperBoundedEnumerable[@sp A] extends PartialPreviousUpperBounded[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}
