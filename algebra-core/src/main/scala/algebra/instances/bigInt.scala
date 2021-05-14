package algebra
package instances

import algebra.ring._

package object bigInt extends BigIntInstances

trait BigIntInstances extends cats.kernel.instances.BigIntInstances {
  implicit val bigIntAlgebra: BigIntAlgebra =
    new BigIntAlgebra
}

class BigIntAlgebra extends EuclideanRing[BigInt] with Serializable {

  val zero: BigInt = BigInt(0)
  val one: BigInt = BigInt(1)

  def plus(a: BigInt, b: BigInt): BigInt = a + b
  def negate(a: BigInt): BigInt = -a
  override def minus(a: BigInt, b: BigInt): BigInt = a - b

  def times(a: BigInt, b: BigInt): BigInt = a * b

  override def pow(a: BigInt, k: Int): BigInt = a.pow(k)

  override def fromInt(n: Int): BigInt = BigInt(n)
  override def fromBigInt(n: BigInt): BigInt = n

  override def lcm(a: BigInt, b: BigInt)(implicit ev: Eq[BigInt]): BigInt =
    if (a.signum == 0 || b.signum == 0) zero else (a / a.gcd(b)) * b
  override def gcd(a: BigInt, b: BigInt)(implicit ev: Eq[BigInt]): BigInt = a.gcd(b)

  def euclideanFunction(a: BigInt): BigInt = a.abs

  override def equotmod(a: BigInt, b: BigInt): (BigInt, BigInt) = {
    val (qt, rt) = a /% b // truncated quotient and remainder
    if (rt.signum >= 0) (qt, rt)
    else if (b.signum > 0) (qt - 1, rt + b)
    else (qt + 1, rt - b)
  }

  def equot(a: BigInt, b: BigInt): BigInt = {
    val (qt, rt) = a /% b // truncated quotient and remainder
    if (rt.signum >= 0) qt
    else if (b.signum > 0) qt - 1
    else qt + 1
  }

  def emod(a: BigInt, b: BigInt): BigInt = {
    val rt = a % b // truncated remainder
    if (rt.signum >= 0) rt
    else if (b > 0) rt + b
    else rt - b
  }

}
