package cats.kernel
package instances

trait BigIntInstances {
  implicit val catsKernelStdOrderForBigInt: Order[BigInt] with Hash[BigInt] with UnboundedEnum[BigInt] =
    new BigIntOrder
  implicit val catsKernelStdGroupForBigInt: CommutativeGroup[BigInt] =
    new BigIntGroup
}

class BigIntGroup extends CommutativeGroup[BigInt] {
  val empty: BigInt = BigInt(0)
  def combine(x: BigInt, y: BigInt): BigInt = x + y
  def inverse(x: BigInt): BigInt = -x
  override def remove(x: BigInt, y: BigInt): BigInt = x - y
}

trait BigIntUnboundedEnum extends UnboundedEnum[BigInt] {
  override def next(a: BigInt): BigInt = a + 1
  override def previous(a: BigInt): BigInt = a - 1
}

class BigIntOrder extends Order[BigInt] with Hash[BigInt] with BigIntUnboundedEnum {

  def hash(x: BigInt): Int = x.hashCode()
  def compare(x: BigInt, y: BigInt): Int = x.compare(y)

  override def eqv(x: BigInt, y: BigInt): Boolean = x == y
  override def neqv(x: BigInt, y: BigInt): Boolean = x != y
  override def gt(x: BigInt, y: BigInt): Boolean = x > y
  override def gteqv(x: BigInt, y: BigInt): Boolean = x >= y
  override def lt(x: BigInt, y: BigInt): Boolean = x < y
  override def lteqv(x: BigInt, y: BigInt): Boolean = x <= y

  override def min(x: BigInt, y: BigInt): BigInt = x.min(y)
  override def max(x: BigInt, y: BigInt): BigInt = x.max(y)

  override def order: Order[BigInt] = this
}
