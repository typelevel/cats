package algebra
package instances

import algebra.lattice._
import algebra.ring._

package object short extends ShortInstances

trait ShortInstances extends cats.kernel.instances.ShortInstances {
  implicit val shortAlgebra: ShortAlgebra =
    new ShortAlgebra

  val ShortMinMaxLattice: BoundedDistributiveLattice[Short] =
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
