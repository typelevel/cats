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

trait DoubleInstances {
  implicit val catsKernelStdOrderForDouble: Order[Double] & Hash[Double] = new DoubleOrder
  implicit val catsKernelStdGroupForDouble: CommutativeGroup[Double] = new DoubleGroup
}

class DoubleGroup extends CommutativeGroup[Double] {
  def combine(x: Double, y: Double): Double = x + y
  def empty: Double = 0d
  def inverse(x: Double): Double = -x
  override def remove(x: Double, y: Double): Double = x - y
}

class DoubleOrder extends Order[Double] with Hash[Double] {

  // Canonicalize +0.0 / -0.0 so Hash agrees with eqv (primitive ==).
  // java.lang.Double.hashCode distinguishes the two zeros via bit patterns.
  def hash(x: Double): Int =
    java.lang.Double.hashCode(if (x == 0.0) 0.0 else x)

  /**
   * Compare using primitive relational operators so +0.0 and -0.0 are equal,
   * matching [[eqv]] / IEEE floating equality. `java.lang.Double.compare`
   * implements a total order that treats them as different, which made
   * `Order[Option[Double]].eqv` disagree with `Order[Double].eqv` (#4807).
   * NaN values fall through to `Double.compare` for a stable total order.
   */
  def compare(x: Double, y: Double): Int =
    if (x < y) -1
    else if (x > y) 1
    else if (x == y) 0
    else java.lang.Double.compare(x, y)

  override def eqv(x: Double, y: Double): Boolean = x == y
  override def neqv(x: Double, y: Double): Boolean = x != y
  override def gt(x: Double, y: Double): Boolean = x > y
  override def gteqv(x: Double, y: Double): Boolean = x >= y
  override def lt(x: Double, y: Double): Boolean = x < y
  override def lteqv(x: Double, y: Double): Boolean = x <= y

  override def min(x: Double, y: Double): Double =
    Math.min(x, y)
  override def max(x: Double, y: Double): Double =
    Math.max(x, y)
}
