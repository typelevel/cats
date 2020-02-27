package cats.kernel
package instances

trait BigDecimalInstances {
  implicit val catsKernelStdOrderForBigDecimal: Order[BigDecimal] with Hash[BigDecimal] =
    new BigDecimalOrder
  implicit val catsKernelStdGroupForBigDecimal: CommutativeGroup[BigDecimal] =
    new BigDecimalGroup
}

/**
 * Note that combining, removing, and inverting `BigDecimal` values will use unlimited precision
 * operations.
 *
 * This matches the behavior of Scala 2.12 and earlier versions, but not Scala 2.13, which means
 * that `+` and `|+|` (or `sum` and `combineAll`) may not agree if you are working with values with
 * limited-precision `MathContext`s.
 */
class BigDecimalGroup extends CommutativeGroup[BigDecimal] {
  val empty: BigDecimal = BigDecimal(0)
  def combine(x: BigDecimal, y: BigDecimal): BigDecimal = new BigDecimal(x.bigDecimal.add(y.bigDecimal), x.mc)
  def inverse(x: BigDecimal): BigDecimal = new BigDecimal(x.bigDecimal.negate(), x.mc)
  override def remove(x: BigDecimal, y: BigDecimal): BigDecimal =
    new BigDecimal(x.bigDecimal.subtract(y.bigDecimal), x.mc)
}

class BigDecimalOrder extends Order[BigDecimal] with Hash[BigDecimal] {

  def hash(x: BigDecimal): Int = x.hashCode()

  def compare(x: BigDecimal, y: BigDecimal): Int = x.compare(y)

  override def eqv(x: BigDecimal, y: BigDecimal): Boolean = x == y
  override def neqv(x: BigDecimal, y: BigDecimal): Boolean = x != y
  override def gt(x: BigDecimal, y: BigDecimal): Boolean = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal): Boolean = x >= y
  override def lt(x: BigDecimal, y: BigDecimal): Boolean = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal): Boolean = x <= y

  override def min(x: BigDecimal, y: BigDecimal): BigDecimal = x.min(y)
  override def max(x: BigDecimal, y: BigDecimal): BigDecimal = x.max(y)
}
