package algebra
package instances

import java.math.MathContext

import algebra.ring._

package object bigDecimal extends BigDecimalInstances

trait BigDecimalInstances extends cats.kernel.instances.BigDecimalInstances {
  implicit val bigDecimalAlgebra: BigDecimalAlgebra = new BigDecimalAlgebra()
}

class BigDecimalAlgebra(mc: MathContext) extends Field[BigDecimal] with Serializable {
  def this() = this(MathContext.UNLIMITED)

  val zero: BigDecimal = BigDecimal(0, mc)
  val one: BigDecimal = BigDecimal(1, mc)

  def plus(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
  def negate(a: BigDecimal): BigDecimal = -a
  override def minus(a: BigDecimal, b: BigDecimal): BigDecimal = a - b

  def times(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
  def div(a: BigDecimal, b: BigDecimal): BigDecimal = a / b

  override def pow(a: BigDecimal, k: Int): BigDecimal = a.pow(k)

  override def fromInt(n: Int): BigDecimal = BigDecimal(n, mc)
  override def fromBigInt(n: BigInt): BigDecimal = BigDecimal(n, mc)
}
