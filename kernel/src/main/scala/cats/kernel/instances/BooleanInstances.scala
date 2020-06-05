package cats.kernel
package instances

trait BooleanInstances {
  implicit val catsKernelStdOrderForBoolean
    : Order[Boolean] with Hash[Boolean] with BoundedEnum[Boolean] =
    new BooleanOrder
}

trait BooleanBounded extends BoundedEnum[Boolean] {
  override def minBound: Boolean = false
  override def maxBound: Boolean = true
  override def partialNext(a: Boolean): Option[Boolean] =
    if(!a) Some(true) else None
  override def partialPrevious(a: Boolean): Option[Boolean] =
    if(a) Some(false) else None
}

class BooleanOrder extends Order[Boolean] with Hash[Boolean] with BooleanBounded { self =>

  def hash(x: Boolean): Int = x.hashCode()
  def compare(x: Boolean, y: Boolean): Int =
    if (x == y) 0 else if (x) 1 else -1

  override def eqv(x: Boolean, y: Boolean): Boolean = x == y
  override def neqv(x: Boolean, y: Boolean): Boolean = x != y
  override def gt(x: Boolean, y: Boolean): Boolean = x && !y
  override def lt(x: Boolean, y: Boolean): Boolean = !x && y
  override def gteqv(x: Boolean, y: Boolean): Boolean = x == y || x
  override def lteqv(x: Boolean, y: Boolean): Boolean = x == y || y

  override def min(x: Boolean, y: Boolean): Boolean = x && y
  override def max(x: Boolean, y: Boolean): Boolean = x || y

  override val order: Order[Boolean] = self
}
