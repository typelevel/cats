package cats.kernel
package instances

import scala.collection.immutable.BitSet

package object bitSet extends BitSetInstances // scalastyle:ignore package.object.name

trait BitSetInstances {
  implicit val catsKernelStdOrderForBitSet: PartialOrder[BitSet] with Hash[BitSet] =
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
