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

trait BooleanInstances {
  implicit val catsKernelStdBoundableEnumerableForBoolean: Order[Boolean] with Hash[Boolean] with BoundableEnumerable[Boolean] =
    new BooleanOrder

  @deprecated(message = "Please use catsKernelStdBoundableEnumerableForBoolean", since = "2.10.0")
  val catsKernelStdOrderForBoolean: Order[Boolean] with Hash[Boolean] with BoundedEnumerable[Boolean] =
    new BooleanOrder
}

@deprecated(message = "Please use BooleanBoundableEnumerable.", since = "2.10.0")
trait BooleanEnumerable extends BoundedEnumerable[Boolean] {
  override def partialNext(a: Boolean): Option[Boolean] =
    if (!a) Some(true) else None
  override def partialPrevious(a: Boolean): Option[Boolean] =
    if (a) Some(false) else None
}

private[instances] trait BooleanBoundableEnumerable extends BoundableEnumerable[Boolean] {
  override final val size: BigInt = BigInt(2)

  override final def fromEnum(a: Boolean): BigInt =
    if (a) {
      BigInt(1)
    } else {
      BigInt(0)
    }

  override final def toEnumOpt(i: BigInt): Option[Boolean] =
    if (i == BigInt(0)) {
      Some(false)
    } else if (i == BigInt(1)) {
      Some(true)
    } else {
      None
    }
}

trait BooleanBounded extends LowerBounded[Boolean] with UpperBounded[Boolean] {
  override def minBound: Boolean = false
  override def maxBound: Boolean = true
}

@nowarn("msg=BooleanBoundableEnumerable")
class BooleanOrder extends Order[Boolean] with Hash[Boolean] with BooleanBounded with BooleanEnumerable with BooleanBoundableEnumerable { self =>

  def hash(x: Boolean): Int = x.hashCode()
  def compare(x: Boolean, y: Boolean): Int =
    if (x == y) 0 else if (x) 1 else -1

  override def eqv(x: Boolean, y: Boolean): Boolean = x == y
  override def neqv(x: Boolean, y: Boolean): Boolean = x != y
  override def gt(x: Boolean, y: Boolean): Boolean = x && !y
  override def lt(x: Boolean, y: Boolean): Boolean = !x && y
  override def gteqv(x: Boolean, y: Boolean): Boolean = x == y || x
  override def lteqv(x: Boolean, y: Boolean): Boolean = x == y || y

  override def min(x: Boolean, y: Boolean): Boolean = x && y
  override def max(x: Boolean, y: Boolean): Boolean = x || y

  override val order: Order[Boolean] = self
}
