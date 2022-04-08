/*
 * Copyright (c) 2022 Typelevel
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

package algebra
package instances

import algebra.lattice.DistributiveLattice
import algebra.ring.Field
import java.lang.Math

trait FloatInstances extends cats.kernel.instances.FloatInstances {
  implicit val floatAlgebra: Field[Float] =
    new FloatAlgebra

  // Not bounded due to the presence of NaN
  val FloatMinMaxLattice: DistributiveLattice[Float] =
    DistributiveLattice.minMax[Float]
}

/**
 * Due to the way floating-point equality works, this instance is not
 * lawful under equality, but is correct when taken as an
 * approximation of an exact value.
 *
 * If you would prefer an absolutely lawful fractional value, you'll
 * need to investigate rational numbers or more exotic types.
 */
class FloatAlgebra extends Field[Float] with Serializable {

  def zero: Float = 0.0f
  def one: Float = 1.0f

  def plus(x: Float, y: Float): Float = x + y
  def negate(x: Float): Float = -x
  override def minus(x: Float, y: Float): Float = x - y

  def times(x: Float, y: Float): Float = x * y
  def div(x: Float, y: Float): Float = x / y
  override def reciprocal(x: Float): Float = 1.0f / x

  override def pow(x: Float, y: Int): Float =
    Math.pow(x.toDouble, y.toDouble).toFloat

  override def fromInt(x: Int): Float = x.toFloat
  override def fromBigInt(n: BigInt): Float = n.toFloat
  override def fromDouble(x: Double): Float = x.toFloat
}
