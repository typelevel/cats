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

import scala.{specialized => sp}

/**
 * A typeclass with an operation which returns a member which is
 * greater or `None` than the one supplied.
 */
trait PartialNext[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialNext(a: A): Option[A]
}

object PartialNext {

  def apply[A](implicit A: PartialNext[A]): PartialNext[A] =
    A

  implicit def catsKernelPartialNextForUnit: PartialNext[Unit] =
    cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelPartialNextForBoolean: PartialNext[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelPartialNextForByte: PartialNext[Byte] =
    cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def catsKernelPartialNextForInt: PartialNext[Int] =
    cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelPartialNextForShort: PartialNext[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelPartialNextForLong: PartialNext[Long] =
    cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelPartialNextForChar: PartialNext[Char] =
    cats.kernel.instances.char.catsKernelStdOrderForChar
  implicit def catsKernelPartialNextForBigInt: PartialNext[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
}

/**
 * A typeclass with an operation which returns a member which is
 * always greater than the one supplied.
 */
trait Next[@sp A] extends PartialNext[A] {
  def next(a: A): A
  override def partialNext(a: A): Option[A] = Some(next(a))
}

object Next {
  def apply[A](implicit A: Next[A]): Next[A] =
    A

  implicit def catsKernelNextForBigInt: Next[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
}

/**
 * A typeclass with an operation which returns a member which is
 * smaller or `None` than the one supplied.
 */
trait PartialPrevious[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialPrevious(a: A): Option[A]
}

object PartialPrevious {
  def apply[A](implicit A: PartialPrevious[A]): PartialPrevious[A] =
    A

  implicit def catsKernelPartialPreviousForUnit: PartialPrevious[Unit] =
    cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelPartialPreviousForBoolean: PartialPrevious[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelPartialPreviousForByte: PartialPrevious[Byte] =
    cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def catsKernelPartialPreviousForInt: PartialPrevious[Int] =
    cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelPartialPreviousForShort: PartialPrevious[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelPartialPreviousForLong: PartialPrevious[Long] =
    cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelPartialPreviousForChar: PartialPrevious[Char] =
    cats.kernel.instances.char.catsKernelStdOrderForChar
  implicit def catsKernelPartialPreviousForBigInt: PartialPrevious[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
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

object Previous {
  def apply[A](implicit A: Previous[A]): Previous[A] =
    A

  implicit def catsKernelPreviousForBigInt: Previous[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
}

/**
 * A typeclass which has both `previous` and `next` operations
 * such that `next . previous == identity`.
 */
trait UnboundedEnumerable[@sp A] extends Next[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

object UnboundedEnumerable {
  def apply[A](implicit A: UnboundedEnumerable[A]): UnboundedEnumerable[A] =
    A
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
