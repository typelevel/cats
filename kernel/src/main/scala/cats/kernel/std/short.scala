package cats.kernel
package std

package object short extends ShortInstances

trait ShortInstances {
  implicit val shortOrder: Order[Short] = new ShortOrder
}

class ShortOrder extends Order[Short] {

  // use java.lang.Short.compare if we can rely on java >= 1.7
  def compare(x: Short, y: Short): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Short, y: Short) = x == y
  override def neqv(x: Short, y: Short) = x != y
  override def gt(x: Short, y: Short) = x > y
  override def gteqv(x: Short, y: Short) = x >= y
  override def lt(x: Short, y: Short) = x < y
  override def lteqv(x: Short, y: Short) = x <= y

  override def min(x: Short, y: Short): Short =
    java.lang.Math.min(x.toInt, y.toInt).toShort
  override def max(x: Short, y: Short): Short =
    java.lang.Math.max(x.toInt, y.toInt).toShort
}
