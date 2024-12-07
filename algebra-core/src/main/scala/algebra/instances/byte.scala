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

package object byte extends ByteInstances

trait ByteInstances extends cats.kernel.instances.ByteInstances {
  implicit val byteAlgebra: ByteAlgebra = new ByteAlgebra

  val ByteMinMaxLattice: BoundedDistributiveLattice[Byte] =
    BoundedDistributiveLattice.minMax[Byte](Byte.MinValue, Byte.MaxValue)
}

class ByteAlgebra extends CommutativeRing[Byte] with Serializable {

  def zero: Byte = 0
  def one: Byte = 1

  def plus(x: Byte, y: Byte): Byte = (x + y).toByte
  def negate(x: Byte): Byte = (-x).toByte
  override def minus(x: Byte, y: Byte): Byte = (x - y).toByte

  def times(x: Byte, y: Byte): Byte = (x * y).toByte

  override def pow(x: Byte, y: Int): Byte =
    Math.pow(x.toDouble, y.toDouble).toByte

  override def fromInt(n: Int): Byte = n.toByte
  override def fromBigInt(n: BigInt): Byte = n.toByte
}
