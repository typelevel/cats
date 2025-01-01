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

package object long extends LongInstances

trait LongInstances extends cats.kernel.instances.LongInstances {
  implicit val longAlgebra: LongAlgebra =
    new LongAlgebra

  val LongMinMaxLattice: BoundedDistributiveLattice[Long] =
    BoundedDistributiveLattice.minMax[Long](Long.MinValue, Long.MaxValue)
}

class LongAlgebra extends CommutativeRing[Long] with Serializable {

  def zero: Long = 0
  def one: Long = 1

  def plus(x: Long, y: Long): Long = x + y
  def negate(x: Long): Long = -x
  override def minus(x: Long, y: Long): Long = x - y

  def times(x: Long, y: Long): Long = x * y

  override def pow(x: Long, y: Int): Long = StaticMethods.pow(x, y.toLong)

  override def fromInt(n: Int): Long = n.toLong
  override def fromBigInt(n: BigInt): Long = n.toLong
}
