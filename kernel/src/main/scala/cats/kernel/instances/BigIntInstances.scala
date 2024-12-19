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

trait BigIntInstances {
  implicit val catsKernelStdOrderForBigInt: Order[BigInt] & Hash[BigInt] & UnboundedEnumerable[BigInt] =
    new BigIntOrder
  implicit val catsKernelStdGroupForBigInt: CommutativeGroup[BigInt] =
    new BigIntGroup
}

class BigIntGroup extends CommutativeGroup[BigInt] {
  val empty: BigInt = BigInt(0)
  def combine(x: BigInt, y: BigInt): BigInt = x + y
  def inverse(x: BigInt): BigInt = -x
  override def remove(x: BigInt, y: BigInt): BigInt = x - y
}

trait BigIntUnboundedEnum extends UnboundedEnumerable[BigInt] {
  override def next(a: BigInt): BigInt = a + 1
  override def previous(a: BigInt): BigInt = a - 1
}

class BigIntOrder extends Order[BigInt] with Hash[BigInt] with BigIntUnboundedEnum {

  def hash(x: BigInt): Int = x.hashCode()
  def compare(x: BigInt, y: BigInt): Int = x.compare(y)

  override def eqv(x: BigInt, y: BigInt): Boolean = x == y
  override def neqv(x: BigInt, y: BigInt): Boolean = x != y
  override def gt(x: BigInt, y: BigInt): Boolean = x > y
  override def gteqv(x: BigInt, y: BigInt): Boolean = x >= y
  override def lt(x: BigInt, y: BigInt): Boolean = x < y
  override def lteqv(x: BigInt, y: BigInt): Boolean = x <= y

  override def min(x: BigInt, y: BigInt): BigInt = x.min(y)
  override def max(x: BigInt, y: BigInt): BigInt = x.max(y)

  override def order: Order[BigInt] = this
}
