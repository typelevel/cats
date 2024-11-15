/*
 * Copyright (c) 2015 Typelevel
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

import java.math.MathContext

import algebra.ring.*

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
