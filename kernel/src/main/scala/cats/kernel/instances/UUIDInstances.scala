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

import java.util.UUID

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] & Hash[UUID] & LowerBounded[UUID] & UpperBounded[UUID] =
    new Order[UUID] with Hash[UUID] with UUIDBounded { self =>
      def compare(x: UUID, y: UUID): Int = x.compareTo(y)
      def hash(x: UUID): Int = x.hashCode()
      val partialOrder: PartialOrder[UUID] = self
    }
}

trait UUIDBounded extends LowerBounded[UUID] with UpperBounded[UUID] {
  override def minBound: UUID = new UUID(Long.MinValue, Long.MinValue)
  override def maxBound: UUID = new UUID(Long.MaxValue, Long.MaxValue)
}
