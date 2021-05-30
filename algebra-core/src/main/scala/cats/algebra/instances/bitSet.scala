package cats
package algebra
package instances

import scala.collection.immutable.BitSet

import cats.algebra.lattice._

object bitSet extends BitSetInstances

trait BitSetInstances {
  implicit val catsAlgebraStdAlgebraForBitSet: BitSetAlgebra =
    new BitSetAlgebra
}

class BitSetAlgebra extends GenBool[BitSet] with Serializable {
  val zero: BitSet = BitSet.empty
  def and(a: BitSet, b: BitSet): BitSet = a & b
  def or(a: BitSet, b: BitSet): BitSet = a | b
  def without(a: BitSet, b: BitSet): BitSet = a -- b
  override def xor(a: BitSet, b: BitSet): BitSet = a ^ b
}
