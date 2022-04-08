/*
 * Copyright (c) 2022 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package algebra
package instances

import algebra.ring._

package object bigInt extends BigIntInstances

trait BigIntInstances extends cats.kernel.instances.BigIntInstances {
  implicit val bigIntAlgebra: BigIntAlgebra = new BigIntTruncatedDivison
  implicit def bigIntTruncatedDivision: TruncatedDivision[BigInt] =
    bigIntAlgebra.asInstanceOf[BigIntTruncatedDivison] // Bin-compat hack to avoid allocation
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

class BigIntTruncatedDivison extends BigIntAlgebra with TruncatedDivision.forCommutativeRing[BigInt] {
  override def tquot(x: BigInt, y: BigInt): BigInt = x / y
  override def tmod(x: BigInt, y: BigInt): BigInt = x % y
  override def order: Order[BigInt] = cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
}
