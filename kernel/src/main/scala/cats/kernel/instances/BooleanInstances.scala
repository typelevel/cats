package cats.kernel
package instances

trait BooleanInstances {
  implicit val catsKernelStdOrderForBoolean: Order[Boolean] with Hash[Boolean] with BoundedEnumerable[Boolean] =
    new BooleanOrder
}

trait BooleanEnumerable extends BoundedEnumerable[Boolean] {
  override def partialNext(a: Boolean): Option[Boolean] =
    if (!a) Some(true) else None
  override def partialPrevious(a: Boolean): Option[Boolean] =
    if (a) Some(false) else None
}

trait BooleanBounded extends LowerBounded[Boolean] with UpperBounded[Boolean] {
  override def minBound: Boolean = false
  override def maxBound: Boolean = true
}

class BooleanOrder extends Order[Boolean] with Hash[Boolean] with BooleanBounded with BooleanEnumerable { self =>

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
