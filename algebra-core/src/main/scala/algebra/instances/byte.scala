package algebra
package instances

import algebra.lattice._
import algebra.ring._

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
