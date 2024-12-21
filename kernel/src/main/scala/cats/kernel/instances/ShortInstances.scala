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

trait ShortInstances {

  implicit val catsKernelStdBoundableEnumerableForShort: Order[Short] with Hash[Short] with BoundableEnumerable[Short] =
    new ShortOrder

  implicit val catsKernelStdGroupForShort: CommutativeGroup[Short] = new ShortGroup

  @deprecated(message = "Please use catsKernelStdBoundableEnumerableForShort", since = "2.10.0")
  val catsKernelStdOrderForShort: Order[Short] with Hash[Short] with BoundedEnumerable[Short] =
    new ShortOrder
}

class ShortGroup extends CommutativeGroup[Short] {
  def combine(x: Short, y: Short): Short = (x + y).toShort
  def empty: Short = 0
  def inverse(x: Short): Short = (-x).toShort
  override def remove(x: Short, y: Short): Short = (x - y).toShort
}

@deprecated(message = "Please use ShortBoundableEnumerable.", since = "2.10.0")
trait ShortEnumerable extends BoundedEnumerable[Short] {
  override def partialNext(a: Short): Option[Short] =
    if (order.eqv(a, maxBound)) None else Some((a + 1).toShort)
  override def partialPrevious(a: Short): Option[Short] =
    if (order.eqv(a, minBound)) None else Some((a - 1).toShort)
}

private[instances] trait ShortBoundableEnumerable extends BoundableEnumerable[Short] {
  override final val size: BigInt =
    (BigInt(maxBound.toInt) - BigInt(minBound.toInt)) + BigInt(1)

  override final def fromEnum(a: Short): BigInt =
    BigInt(a.toInt)

  override final def toEnumOpt(i: BigInt): Option[Short] =
    if (i >= BigInt(minBound.toInt) && i <= BigInt(maxBound.toInt)) {
      Some(i.toShort)
    } else {
      None
    }
}

trait ShortBounded extends LowerBounded[Short] with UpperBounded[Short] {
  override def minBound: Short = Short.MinValue
  override def maxBound: Short = Short.MaxValue
}

@nowarn("msg=ShortBoundableEnumerable")
class ShortOrder extends Order[Short] with Hash[Short] with ShortBounded with ShortEnumerable with ShortBoundableEnumerable { self =>

  def hash(x: Short): Int = x.hashCode()
  // use java.lang.Short.compare if we can rely on java >= 1.7
  def compare(x: Short, y: Short): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Short, y: Short): Boolean = x == y
  override def neqv(x: Short, y: Short): Boolean = x != y
  override def gt(x: Short, y: Short): Boolean = x > y
  override def gteqv(x: Short, y: Short): Boolean = x >= y
  override def lt(x: Short, y: Short): Boolean = x < y
  override def lteqv(x: Short, y: Short): Boolean = x <= y

  override def min(x: Short, y: Short): Short =
    java.lang.Math.min(x.toInt, y.toInt).toShort
  override def max(x: Short, y: Short): Short =
    java.lang.Math.max(x.toInt, y.toInt).toShort

  override val order: Order[Short] = self
}
