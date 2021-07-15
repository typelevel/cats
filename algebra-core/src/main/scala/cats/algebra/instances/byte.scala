package cats
package algebra
package instances

import cats.algebra.lattice._
import cats.algebra.ring._

object byte extends ByteInstances

trait ByteInstances {
  implicit val catsAlgebraStdAlgebraForByte: ByteAlgebra = new ByteAlgebra

  val catsAlgebraStdMinMaxLatticeForByte: BoundedDistributiveLattice[Byte] =
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
