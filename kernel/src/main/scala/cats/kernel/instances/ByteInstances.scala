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

trait ByteInstances {
  implicit val catsKernelStdBoundableEnumerableForByte: Order[Byte] with Hash[Byte] with BoundableEnumerable[Byte] =
    new ByteOrder
  implicit val catsKernelStdGroupForByte: CommutativeGroup[Byte] = new ByteGroup

  @deprecated(message = "Please use catsKernelStdBoundableEnumerableForByte", since = "2.10.0")
  val catsKernelStdOrderForByte: Order[Byte] with Hash[Byte] with BoundedEnumerable[Byte] =
    new ByteOrder
}

class ByteGroup extends CommutativeGroup[Byte] {
  def combine(x: Byte, y: Byte): Byte = (x + y).toByte
  def empty: Byte = 0
  def inverse(x: Byte): Byte = (-x).toByte
  override def remove(x: Byte, y: Byte): Byte = (x - y).toByte
}

@deprecated(message = "Please use ByteBoundableEnumerable.", since = "2.10.0")
trait ByteEnumerable extends BoundedEnumerable[Byte] {
  override def partialNext(a: Byte): Option[Byte] =
    if (order.eqv(a, maxBound)) None else Some((a + 1).toByte)
  override def partialPrevious(a: Byte): Option[Byte] =
    if (order.eqv(a, minBound)) None else Some((a - 1).toByte)
}

private[instances] trait ByteBoundableEnumerable extends BoundableEnumerable[Byte] {
  override final def size: BigInt =
    (BigInt(minBound.toInt) - BigInt(maxBound.toInt)) + BigInt(1)

  override final def fromEnum(a: Byte): BigInt =
    BigInt(a.toInt)

  override final def toEnumOpt(i: BigInt): Option[Byte] =
    if (i >= BigInt(minBound.toInt) && i <= BigInt(maxBound.toInt)) {
      Some(i.toByte)
    } else {
      None
    }
}

trait ByteBounded extends LowerBounded[Byte] with UpperBounded[Byte] {
  override def minBound: Byte = Byte.MinValue
  override def maxBound: Byte = Byte.MaxValue
}

@nowarn("msg=ByteBoundableEnumerable")
class ByteOrder extends Order[Byte] with Hash[Byte] with ByteBounded with ByteEnumerable with ByteBoundableEnumerable { self =>

  def hash(x: Byte): Int = x.hashCode()

  def compare(x: Byte, y: Byte): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Byte, y: Byte): Boolean = x == y
  override def neqv(x: Byte, y: Byte): Boolean = x != y
  override def gt(x: Byte, y: Byte): Boolean = x > y
  override def gteqv(x: Byte, y: Byte): Boolean = x >= y
  override def lt(x: Byte, y: Byte): Boolean = x < y
  override def lteqv(x: Byte, y: Byte): Boolean = x <= y

  override def min(x: Byte, y: Byte): Byte =
    java.lang.Math.min(x.toInt, y.toInt).toByte
  override def max(x: Byte, y: Byte): Byte =
    java.lang.Math.max(x.toInt, y.toInt).toByte

  override val order: Order[Byte] = self
}
