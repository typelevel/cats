package cats.kernel
package std

package object boolean extends BooleanInstances

trait BooleanInstances {
  implicit val catsKernelStdOrderForBoolean: Order[Boolean] =
    new BooleanOrder
}

class BooleanOrder extends Order[Boolean] {
  def compare(x: Boolean, y: Boolean): Int =
    if (x == y) 0 else if (x) 1 else -1

  override def eqv(x:Boolean, y:Boolean): Boolean = x == y
  override def neqv(x:Boolean, y:Boolean): Boolean = x != y
  override def gt(x: Boolean, y: Boolean): Boolean = x && !y
  override def lt(x: Boolean, y: Boolean): Boolean = !x && y
  override def gteqv(x: Boolean, y: Boolean): Boolean = x == y || x
  override def lteqv(x: Boolean, y: Boolean): Boolean = x == y || y

  override def min(x: Boolean, y: Boolean): Boolean = x && y
  override def max(x: Boolean, y: Boolean): Boolean = x || y
}
