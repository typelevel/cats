package cats.kernel
package instances

package object double extends DoubleInstances

trait DoubleInstances {
  implicit val catsKernelStdOrderForDouble: Order[Double] with Hash[Double] = new DoubleOrder
  implicit val catsKernelStdGroupForDouble: CommutativeGroup[Double] = new DoubleGroup
}

class DoubleGroup extends CommutativeGroup[Double] {
  def combine(x: Double, y: Double): Double = x + y
  def empty: Double = 0d
  def inverse(x: Double): Double = -x
  override def remove(x: Double, y: Double): Double = x - y
}

class DoubleOrder extends Order[Double] with Hash[Double] {

  def hash(x: Double): Int = x.hashCode()
  def compare(x: Double, y: Double): Int =
    java.lang.Double.compare(x, y)

  override def eqv(x: Double, y: Double): Boolean = x == y
  override def neqv(x: Double, y: Double): Boolean = x != y
  override def gt(x: Double, y: Double): Boolean = x > y
  override def gteqv(x: Double, y: Double): Boolean = x >= y
  override def lt(x: Double, y: Double): Boolean = x < y
  override def lteqv(x: Double, y: Double): Boolean = x <= y

  override def min(x: Double, y: Double): Double =
    Math.min(x, y)
  override def max(x: Double, y: Double): Double =
    Math.max(x, y)
}
