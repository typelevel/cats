package cats
package algebra
package instances

import cats.algebra.lattice._
import cats.algebra.ring._

object short extends ShortInstances

trait ShortInstances {
  implicit val catsAlgebraStdAlgebraForShort: ShortAlgebra =
    new ShortAlgebra

  val catsAlgebraStdMinMaxLatticeForShort: BoundedDistributiveLattice[Short] =
    BoundedDistributiveLattice.minMax[Short](Short.MinValue, Short.MaxValue)
}

class ShortAlgebra extends CommutativeRing[Short] with Serializable {

  def zero: Short = 0
  def one: Short = 1

  def plus(x: Short, y: Short): Short = (x + y).toShort
  def negate(x: Short): Short = (-x).toShort
  override def minus(x: Short, y: Short): Short = (x - y).toShort

  def times(x: Short, y: Short): Short = (x * y).toShort

  override def pow(x: Short, y: Int): Short =
    Math.pow(x.toDouble, y.toDouble).toShort

  override def fromInt(n: Int): Short = n.toShort
  override def fromBigInt(n: BigInt): Short = n.toShort
}
