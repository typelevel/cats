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

import scala.concurrent.duration.Duration

trait DurationInstances {
  implicit val catsKernelStdOrderForDuration
    : Order[Duration] & Hash[Duration] & LowerBounded[Duration] & UpperBounded[Duration] = new DurationOrder
  implicit val catsKernelStdGroupForDuration: CommutativeGroup[Duration] = new DurationGroup
}

// Duration.Undefined, Duration.Inf, Duration.MinusInf

trait DurationBounded extends LowerBounded[Duration] with UpperBounded[Duration] {
  override def minBound: Duration = Duration.MinusInf
  override def maxBound: Duration = Duration.Inf
}

/**
 * This ordering is valid for all defined durations.
 *
 * The value Duration.Undefined breaks our laws, because undefined
 * values are not equal to themselves.
 */
class DurationOrder extends Order[Duration] with Hash[Duration] with DurationBounded { self =>
  def hash(x: Duration): Int = x.hashCode()

  def compare(x: Duration, y: Duration): Int = x.compare(y)

  override def eqv(x: Duration, y: Duration): Boolean = x == y
  override def neqv(x: Duration, y: Duration): Boolean = x != y
  override def gt(x: Duration, y: Duration): Boolean = x > y
  override def gteqv(x: Duration, y: Duration): Boolean = x >= y
  override def lt(x: Duration, y: Duration): Boolean = x < y
  override def lteqv(x: Duration, y: Duration): Boolean = x <= y

  override def min(x: Duration, y: Duration): Duration = x.min(y)
  override def max(x: Duration, y: Duration): Duration = x.max(y)

  override val partialOrder: PartialOrder[Duration] = self
}

/**
 * This group models addition, but has a few problematic edge cases.
 *
 *   1. finite values can overflow, throwing an exception
 *   2. inf + (-inf) = undefined, not zero
 *   3. undefined + zero = undefined
 */
class DurationGroup extends CommutativeGroup[Duration] {
  def empty: Duration = Duration.Zero
  def inverse(x: Duration): Duration = -x
  def combine(x: Duration, y: Duration): Duration = x + y
  override def remove(x: Duration, y: Duration): Duration = x - y
}
