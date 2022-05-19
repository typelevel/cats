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

package algebra
package instances

import algebra.lattice.DistributiveLattice
import algebra.ring.Field

import java.lang.Math

trait DoubleInstances extends cats.kernel.instances.DoubleInstances {
  implicit val doubleAlgebra: Field[Double] =
    new DoubleAlgebra

  // This is not Bounded due to the presence of NaN
  val DoubleMinMaxLattice: DistributiveLattice[Double] =
    DistributiveLattice.minMax[Double]
}

/**
 * Due to the way floating-point equality works, this instance is not
 * lawful under equality, but is correct when taken as an
 * approximation of an exact value.
 *
 * If you would prefer an absolutely lawful fractional value, you'll
 * need to investigate rational numbers or more exotic types.
 */
class DoubleAlgebra extends Field[Double] with Serializable {

  def zero: Double = 0.0
  def one: Double = 1.0

  def plus(x: Double, y: Double): Double = x + y
  def negate(x: Double): Double = -x
  override def minus(x: Double, y: Double): Double = x - y

  def times(x: Double, y: Double): Double = x * y
  def div(x: Double, y: Double): Double = x / y
  override def reciprocal(x: Double): Double = 1.0 / x
  override def pow(x: Double, y: Int): Double = Math.pow(x, y.toDouble)

  override def fromInt(x: Int): Double = x.toDouble
  override def fromBigInt(n: BigInt): Double = n.toDouble
  override def fromDouble(x: Double): Double = x
}
