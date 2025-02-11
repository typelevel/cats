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

import algebra.lattice.*
import algebra.ring.*

package object int extends IntInstances

trait IntInstances extends cats.kernel.instances.IntInstances {
  implicit val intAlgebra: IntAlgebra =
    new IntAlgebra

  val IntMinMaxLattice: BoundedDistributiveLattice[Int] =
    BoundedDistributiveLattice.minMax[Int](Int.MinValue, Int.MaxValue)
}

class IntAlgebra extends CommutativeRing[Int] with Serializable {

  def zero: Int = 0
  def one: Int = 1

  def plus(x: Int, y: Int): Int = x + y
  def negate(x: Int): Int = -x
  override def minus(x: Int, y: Int): Int = x - y

  def times(x: Int, y: Int): Int = x * y

  override def pow(x: Int, y: Int): Int =
    StaticMethods.pow(x.toLong, y.toLong).toInt

  override def fromInt(n: Int): Int = n
  override def fromBigInt(n: BigInt): Int = n.toInt
}
