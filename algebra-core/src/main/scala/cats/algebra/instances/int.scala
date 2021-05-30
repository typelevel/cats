package cats
package algebra
package instances

import cats.algebra.lattice._
import cats.algebra.ring._

object int extends IntInstances

trait IntInstances {
  implicit val catsAlgebraStdAlgebraForInt: IntAlgebra =
    new IntAlgebra

  val catsAlgebraStdBoundedDistributiveLatticeForInt: BoundedDistributiveLattice[Int] =
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
