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

trait FloatInstances {
  implicit val catsKernelStdOrderForFloat: Order[Float] & Hash[Float] = new FloatOrder
  implicit val catsKernelStdGroupForFloat: CommutativeGroup[Float] = new FloatGroup
}

/**
 * This is only approximately associative.
 */
class FloatGroup extends CommutativeGroup[Float] {
  def combine(x: Float, y: Float): Float = x + y
  def empty: Float = 0f
  def inverse(x: Float): Float = -x
  override def remove(x: Float, y: Float): Float = x - y
}

/**
 * Due to the way floating-point equality works, this instance is not
 * lawful under equality, but is correct when taken as an
 * approximation of an exact value.
 *
 * If you would prefer an absolutely lawful fractional value, you'll
 * need to investigate rational numbers or more exotic types.
 */
class FloatOrder extends Order[Float] with Hash[Float] {

  def hash(x: Float): Int = x.hashCode()

  def compare(x: Float, y: Float): Int =
    java.lang.Float.compare(x, y)

  override def eqv(x: Float, y: Float): Boolean = x == y
  override def neqv(x: Float, y: Float): Boolean = x != y
  override def gt(x: Float, y: Float): Boolean = x > y
  override def gteqv(x: Float, y: Float): Boolean = x >= y
  override def lt(x: Float, y: Float): Boolean = x < y
  override def lteqv(x: Float, y: Float): Boolean = x <= y

  override def min(x: Float, y: Float): Float =
    java.lang.Math.min(x, y)
  override def max(x: Float, y: Float): Float =
    java.lang.Math.max(x, y)
}
