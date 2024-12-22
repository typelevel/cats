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

trait CharInstances {
  implicit val catsKernelStdOrderForChar: CharOrder & Hash[Char] & BoundedEnumerable[Char] = new CharOrder
}

trait CharEnumerable extends BoundedEnumerable[Char] {
  override def partialNext(a: Char): Option[Char] =
    if (a == maxBound) None else Some((a + 1).toChar)
  override def partialPrevious(a: Char): Option[Char] =
    if (a == minBound) None else Some((a - 1).toChar)
}

trait CharBounded extends LowerBounded[Char] with UpperBounded[Char] {
  override def minBound: Char = Char.MinValue
  override def maxBound: Char = Char.MaxValue
}

class CharOrder extends Order[Char] with Hash[Char] with CharBounded with CharEnumerable { self =>
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
