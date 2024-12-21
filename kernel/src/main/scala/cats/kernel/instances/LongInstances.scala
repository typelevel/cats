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

trait LongInstances {
  implicit val catsKernelStdOrderForLong: Order[Long] & Hash[Long] & BoundedEnumerable[Long] =
    new LongOrder
  implicit val catsKernelStdGroupForLong: CommutativeGroup[Long] = new LongGroup
}

class LongGroup extends CommutativeGroup[Long] {
  def combine(x: Long, y: Long): Long = x + y
  def empty: Long = 0L
  def inverse(x: Long): Long = -x
  override def remove(x: Long, y: Long): Long = x - y
}

trait LongEnumerable extends BoundedEnumerable[Long] {
  override def partialNext(a: Long): Option[Long] =
    if (order.neqv(a, maxBound)) Some(a + 1L) else None
  override def partialPrevious(a: Long): Option[Long] =
    if (order.neqv(a, minBound)) Some(a - 1L) else None
}

trait LongBounded extends UpperBounded[Long] with LowerBounded[Long] {
  override def minBound: Long = Long.MinValue
  override def maxBound: Long = Long.MaxValue
}

class LongOrder extends Order[Long] with Hash[Long] with LongBounded with LongEnumerable { self =>

  def hash(x: Long): Int = x.hashCode()

  // use java.lang.Long.compare if we can rely on java >= 1.7
  def compare(x: Long, y: Long): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Long, y: Long): Boolean = x == y
  override def neqv(x: Long, y: Long): Boolean = x != y
  override def gt(x: Long, y: Long): Boolean = x > y
  override def gteqv(x: Long, y: Long): Boolean = x >= y
  override def lt(x: Long, y: Long): Boolean = x < y
  override def lteqv(x: Long, y: Long): Boolean = x <= y

  override def min(x: Long, y: Long): Long =
    java.lang.Math.min(x, y)
  override def max(x: Long, y: Long): Long =
    java.lang.Math.max(x, y)

  override val order: Order[Long] = self
}
