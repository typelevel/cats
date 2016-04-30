package cats.kernel
package std

package object long extends LongInstances

trait LongInstances {
  implicit val longOrder: Order[Long] = new LongOrder
  implicit val longGroup: CommutativeGroup[Long] = new LongGroup
}

class LongGroup extends CommutativeGroup[Long] {
  def combine(x: Long, y: Long): Long = x + y
  def empty: Long = 0L
  def inverse(x: Long): Long = -x
  override def remove(x: Long, y: Long): Long = x - y
}

class LongOrder extends Order[Long] {

  // use java.lang.Long.compare if we can rely on java >= 1.7
  def compare(x: Long, y: Long): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Long, y: Long) = x == y
  override def neqv(x: Long, y: Long) = x != y
  override def gt(x: Long, y: Long) = x > y
  override def gteqv(x: Long, y: Long) = x >= y
  override def lt(x: Long, y: Long) = x < y
  override def lteqv(x: Long, y: Long) = x <= y

  override def min(x: Long, y: Long): Long =
    java.lang.Math.min(x, y)
  override def max(x: Long, y: Long): Long =
    java.lang.Math.max(x, y)
}
