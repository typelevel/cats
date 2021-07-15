package cats
package algebra
package instances

import cats.algebra.lattice._
import cats.algebra.ring._

object long extends LongInstances

trait LongInstances {
  implicit val catsAlgebraStdAlgebraForLong: LongAlgebra =
    new LongAlgebra

  val catsAlgebraStdBoundedDistributiveLatticeForLong: BoundedDistributiveLattice[Long] =
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
