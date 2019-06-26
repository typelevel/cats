package cats.kernel
package instances

trait ByteInstances {
  implicit val catsKernelStdOrderForByte: Order[Byte] with Hash[Byte] with LowerBounded[Byte] with UpperBounded[Byte] =
    new ByteOrder
  implicit val catsKernelStdGroupForByte: CommutativeGroup[Byte] = new ByteGroup
}

class ByteGroup extends CommutativeGroup[Byte] {
  def combine(x: Byte, y: Byte): Byte = (x + y).toByte
  def empty: Byte = 0
  def inverse(x: Byte): Byte = (-x).toByte
  override def remove(x: Byte, y: Byte): Byte = (x - y).toByte
}

trait ByteBounded extends LowerBounded[Byte] with UpperBounded[Byte] {
  override def minBound: Byte = Byte.MinValue
  override def maxBound: Byte = Byte.MaxValue
}

class ByteOrder extends Order[Byte] with Hash[Byte] with ByteBounded { self =>

  def hash(x: Byte): Int = x.hashCode()

  def compare(x: Byte, y: Byte): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Byte, y: Byte): Boolean = x == y
  override def neqv(x: Byte, y: Byte): Boolean = x != y
  override def gt(x: Byte, y: Byte): Boolean = x > y
  override def gteqv(x: Byte, y: Byte): Boolean = x >= y
  override def lt(x: Byte, y: Byte): Boolean = x < y
  override def lteqv(x: Byte, y: Byte): Boolean = x <= y

  override def min(x: Byte, y: Byte): Byte =
    java.lang.Math.min(x.toInt, y.toInt).toByte
  override def max(x: Byte, y: Byte): Byte =
    java.lang.Math.max(x.toInt, y.toInt).toByte

  override val partialOrder: PartialOrder[Byte] = self
}
