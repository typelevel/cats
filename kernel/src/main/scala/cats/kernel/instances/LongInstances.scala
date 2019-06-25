package cats.kernel
package instances

trait LongInstances {
  implicit val catsKernelStdOrderForLong: Order[Long] with Hash[Long] with LowerBounded[Long] with UpperBounded[Long] =
    new LongOrder
  implicit val catsKernelStdGroupForLong: CommutativeGroup[Long] = new LongGroup
}

class LongGroup extends CommutativeGroup[Long] {
  def combine(x: Long, y: Long): Long = x + y
  def empty: Long = 0L
  def inverse(x: Long): Long = -x
  override def remove(x: Long, y: Long): Long = x - y
}

trait LongBounded extends LowerBounded[Long] with UpperBounded[Long] {
  override def minBound: Long = Long.MinValue
  override def maxBound: Long = Long.MaxValue
}

class LongOrder extends Order[Long] with Hash[Long] with LongBounded { self =>

  def hash(x: Long): Int = x.hashCode()

  // use java.lang.Long.compare if we can rely on java >= 1.7
  def compare(x: Long, y: Long): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Long, y: Long): Boolean = x == y
  override def neqv(x: Long, y: Long): Boolean = x != y
  override def gt(x: Long, y: Long): Boolean = x > y
  override def gteqv(x: Long, y: Long): Boolean = x >= y
  override def lt(x: Long, y: Long): Boolean = x < y
  override def lteqv(x: Long, y: Long): Boolean = x <= y

  override def min(x: Long, y: Long): Long =
    java.lang.Math.min(x, y)
  override def max(x: Long, y: Long): Long =
    java.lang.Math.max(x, y)

  override val partialOrder: PartialOrder[Long] = self
}
