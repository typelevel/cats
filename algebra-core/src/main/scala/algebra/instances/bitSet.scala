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

import scala.collection.immutable.BitSet

import algebra.lattice.*

package object bitSet extends BitSetInstances

trait BitSetInstances extends cats.kernel.instances.BitSetInstances {
  implicit val bitSetAlgebra: BitSetAlgebra =
    new BitSetAlgebra
}

class BitSetAlgebra extends GenBool[BitSet] with Serializable {
  val zero: BitSet = BitSet.empty
  def and(a: BitSet, b: BitSet): BitSet = a & b
  def or(a: BitSet, b: BitSet): BitSet = a | b
  def without(a: BitSet, b: BitSet): BitSet = a -- b
  override def xor(a: BitSet, b: BitSet): BitSet = a ^ b
}
