package cats
package algebra
package instances

import cats.algebra.lattice._
import cats.algebra.ring._

package object int extends IntInstances

trait IntInstances extends cats.kernel.instances.IntInstances {
  implicit val intAlgebra: IntAlgebra =
    new IntAlgebra

  val IntMinMaxLattice: BoundedDistributiveLattice[Int] =
    BoundedDistributiveLattice.minMax[Int](Int.MinValue, Int.MaxValue)
}

class IntAlgebra extends CommutativeRing[Int] with Serializable {

  def zero: Int = 0
  def one: Int = 1

  def plus(x: Int, y: Int): Int = x + y
  def negate(x: Int): Int = -x
  override def minus(x: Int, y: Int): Int = x - y

  def times(x: Int, y: Int): Int = x * y

  override def pow(x: Int, y: Int): Int =
    StaticMethods.pow(x.toLong, y.toLong).toInt

  override def fromInt(n: Int): Int = n
  override def fromBigInt(n: BigInt): Int = n.toInt
}
