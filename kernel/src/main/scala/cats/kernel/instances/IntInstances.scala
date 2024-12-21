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
package instances

import scala.annotation.nowarn

trait IntInstances {
  implicit val catsKernelStdBoundableEnumerableForInt: Order[Int] with Hash[Int] with BoundableEnumerable[Int] =
    new IntOrder

  implicit val catsKernelStdGroupForInt: CommutativeGroup[Int] = new IntGroup

  @deprecated(message = "Please use catsKernelStdBoundableEnumerableForInt instead.", since = "2.10.0")
  val catsKernelStdOrderForInt: Order[Int] with Hash[Int] with BoundedEnumerable[Int] with Enumerable[Int] =
    new IntOrder
}

class IntGroup extends CommutativeGroup[Int] {
  def combine(x: Int, y: Int): Int = x + y
  def empty: Int = 0
  def inverse(x: Int): Int = -x
  override def remove(x: Int, y: Int): Int = x - y
}

@deprecated(message = "Please use IntBoundableEnumerable.", since = "2.10.0")
trait IntEnumerable extends BoundedEnumerable[Int] {
  override def partialNext(a: Int): Option[Int] =
    if (order.eqv(a, maxBound)) None else Some(a + 1)
  override def partialPrevious(a: Int): Option[Int] =
    if (order.eqv(a, minBound)) None else Some(a - 1)
}

private[instances] trait IntBoundableEnumerable extends BoundableEnumerable[Int] {
  override final def size: BigInt =
    (BigInt(maxBound) - BigInt(minBound)) + BigInt(1)

  override final def fromEnum(a: Int): BigInt = BigInt(a)

  override final def toEnumOpt(i: BigInt): Option[Int] =
    if (i >= BigInt(minBound) && i <= BigInt(maxBound)) {
      Some(i.toInt)
    } else {
      None
    }
}

trait IntBounded extends LowerBounded[Int] with UpperBounded[Int] {
  override def minBound: Int = Int.MinValue
  override def maxBound: Int = Int.MaxValue
}

@nowarn("msg=IntBoundableEnumerable")
class IntOrder extends Order[Int] with Hash[Int] with IntBounded with IntEnumerable with IntBoundableEnumerable { self =>
  def hash(x: Int): Int = x.hashCode()
  def compare(x: Int, y: Int): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Int, y: Int): Boolean = x == y
  override def neqv(x: Int, y: Int): Boolean = x != y
  override def gt(x: Int, y: Int): Boolean = x > y
  override def gteqv(x: Int, y: Int): Boolean = x >= y
  override def lt(x: Int, y: Int): Boolean = x < y
  override def lteqv(x: Int, y: Int): Boolean = x <= y

  override def min(x: Int, y: Int): Int =
    java.lang.Math.min(x, y)
  override def max(x: Int, y: Int): Int =
    java.lang.Math.max(x, y)

  override val order: Order[Int] = self
}
