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

trait CharInstances {
  implicit val catsKernelStdBoundableEnumerableForChar: CharOrder with Hash[Char] with BoundableEnumerable[Char] =
    new CharOrder

  @deprecated(message = "Please use catsKernelStdBoundableEnumerableForChar instead.", since = "2.10.0")
  def catsKernelStdOrderForChar: CharOrder with Hash[Char] with BoundedEnumerable[Char] =
    catsKernelStdBoundableEnumerableForChar
}

@deprecated(message = "Please use CharBoundableEnumerable.", since = "2.10.0")
trait CharEnumerable extends BoundedEnumerable[Char] {
  override def partialNext(a: Char): Option[Char] =
    if (a == maxBound) None else Some((a + 1).toChar)
  override def partialPrevious(a: Char): Option[Char] =
    if (a == minBound) None else Some((a - 1).toChar)
}

private[instances] trait CharBoundableEnumerable extends BoundableEnumerable[Char] {
  override final def size: BigInt =
    BigInt(maxBound.toInt)

  override final def fromEnum(a: Char): BigInt =
    BigInt(a.toInt)

  override final def toEnumOpt(i: BigInt): Option[Char] =
    if (i >= BigInt(minBound.toInt) && i <= BigInt(maxBound.toInt)) {
      Some(i.toChar)
    } else {
      None
    }
}

trait CharBounded extends LowerBounded[Char] with UpperBounded[Char] {
  override def minBound: Char = Char.MinValue
  override def maxBound: Char = Char.MaxValue
}

@nowarn("msg=CharBoundableEnumerable")
class CharOrder extends Order[Char] with Hash[Char] with CharBounded with CharEnumerable with CharBoundableEnumerable { self =>
  def hash(x: Char): Int = x.hashCode()
  def compare(x: Char, y: Char): Int =
    if (x < y) -1 else if (x > y) 1 else 0
  override def eqv(x: Char, y: Char): Boolean = x == y
  override def neqv(x: Char, y: Char): Boolean = x != y
  override def gt(x: Char, y: Char): Boolean = x > y
  override def gteqv(x: Char, y: Char): Boolean = x >= y
  override def lt(x: Char, y: Char): Boolean = x < y
  override def lteqv(x: Char, y: Char): Boolean = x <= y

  override val order: Order[Char] = self
}
