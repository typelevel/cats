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

import scala.collection.immutable.BitSet

trait BitSetInstances {
  implicit val catsKernelStdOrderForBitSet: PartialOrder[BitSet] & Hash[BitSet] =
    new BitSetPartialOrder

  implicit val catsKernelStdSemilatticeForBitSet: BoundedSemilattice[BitSet] =
    new BitSetSemilattice
}

class BitSetPartialOrder extends PartialOrder[BitSet] with Hash[BitSet] {
  def hash(x: BitSet): Int = x.hashCode()

  def partialCompare(x: BitSet, y: BitSet): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  override def eqv(x: BitSet, y: BitSet): Boolean =
    x == y
}

class BitSetSemilattice extends BoundedSemilattice[BitSet] {
  def empty: BitSet = BitSet.empty
  def combine(x: BitSet, y: BitSet): BitSet = x | y
}
