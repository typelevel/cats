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

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.{Duration, FiniteDuration}

trait FiniteDurationInstances {
  implicit val catsKernelStdOrderForFiniteDuration
    : Order[FiniteDuration] & Hash[FiniteDuration] & LowerBounded[FiniteDuration] & UpperBounded[FiniteDuration] =
    new FiniteDurationOrder
  implicit val catsKernelStdGroupForFiniteDuration: CommutativeGroup[FiniteDuration] = new FiniteDurationGroup
}

trait FiniteDurationBounded extends LowerBounded[FiniteDuration] with UpperBounded[FiniteDuration] {
  override def minBound: FiniteDuration = FiniteDuration(-Long.MaxValue, TimeUnit.NANOSECONDS)
  override def maxBound: FiniteDuration = FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS)
}

class FiniteDurationOrder extends Order[FiniteDuration] with Hash[FiniteDuration] with FiniteDurationBounded { self =>
  def hash(x: FiniteDuration): Int = x.hashCode()

  def compare(x: FiniteDuration, y: FiniteDuration): Int = x.compare(y)

  override def eqv(x: FiniteDuration, y: FiniteDuration): Boolean = x == y
  override def neqv(x: FiniteDuration, y: FiniteDuration): Boolean = x != y
  override def gt(x: FiniteDuration, y: FiniteDuration): Boolean = x > y
  override def gteqv(x: FiniteDuration, y: FiniteDuration): Boolean = x >= y
  override def lt(x: FiniteDuration, y: FiniteDuration): Boolean = x < y
  override def lteqv(x: FiniteDuration, y: FiniteDuration): Boolean = x <= y

  override def min(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x.min(y)
  override def max(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x.max(y)

  override val partialOrder: PartialOrder[FiniteDuration] = self
}

class FiniteDurationGroup extends CommutativeGroup[FiniteDuration] {
  def empty: FiniteDuration = Duration.Zero
  def inverse(x: FiniteDuration): FiniteDuration = -x
  def combine(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x + y
  override def remove(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x - y
}
