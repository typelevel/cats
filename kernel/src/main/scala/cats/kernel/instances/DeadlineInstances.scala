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
import scala.concurrent.duration.{Deadline, FiniteDuration}

trait DeadlineInstances {
  implicit val catsKernelStdOrderForDeadline
    : Order[Deadline] & Hash[Deadline] & LowerBounded[Deadline] & UpperBounded[Deadline] = new DeadlineOrder
}

trait DeadlineBounded extends LowerBounded[Deadline] with UpperBounded[Deadline] {
  override def minBound: Deadline = Deadline(FiniteDuration(-Long.MaxValue, TimeUnit.NANOSECONDS))
  override def maxBound: Deadline = Deadline(FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS))
}

class DeadlineOrder extends Order[Deadline] with Hash[Deadline] with DeadlineBounded { self =>

  def hash(x: Deadline): Int = x.hashCode()
  def compare(x: Deadline, y: Deadline): Int =
    if (x == y) 0 else if (x > y) 1 else -1

  override def eqv(x: Deadline, y: Deadline): Boolean = x == y
  override def neqv(x: Deadline, y: Deadline): Boolean = x != y
  override def gt(x: Deadline, y: Deadline): Boolean = x > y && x != y
  override def lt(x: Deadline, y: Deadline): Boolean = x < y && x != y
  override def gteqv(x: Deadline, y: Deadline): Boolean = x == y || x > y
  override def lteqv(x: Deadline, y: Deadline): Boolean = x == y || y > x

  override def min(x: Deadline, y: Deadline): Deadline = if (x < y) x else y
  override def max(x: Deadline, y: Deadline): Deadline = if (x > y) x else y

  override val partialOrder: PartialOrder[Deadline] = self
}
