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
package laws

import algebra.lattice.DistributiveLattice
import algebra.ring.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class Rat(val num: BigInt, val den: BigInt) extends Serializable { lhs =>

  override def toString: String =
    if (den == 1) s"$num" else s"$num/$den"

  override def equals(that: Any): Boolean =
    that match {
      case r: Rat => num == r.num && den == r.den
      case _      => false
    }

  override def hashCode(): Int = (num, den).##

  def isZero: Boolean = num == 0

  def isOne: Boolean = num == 1 && den == 1

  def compare(rhs: Rat): Int =
    (lhs.num * rhs.den).compare(rhs.num * lhs.den)

  def abs: Rat = Rat(num.abs, den)

  def signum: Int = num.signum

  def +(rhs: Rat): Rat =
    Rat((lhs.num * rhs.den) + (rhs.num * lhs.den), lhs.den * rhs.den)

  def unary_- : Rat =
    Rat(-lhs.num, lhs.den)

  def *(rhs: Rat): Rat =
    Rat(lhs.num * rhs.num, lhs.den * rhs.den)

  def /~(rhs: Rat) = lhs / rhs

  def %(rhs: Rat) = Rat.Zero

  def reciprocal: Rat =
    if (num == 0) throw new ArithmeticException("/0") else Rat(den, num)

  def /(rhs: Rat): Rat =
    lhs * rhs.reciprocal

  def **(k: Int): Rat =
    Rat(num.pow(k), den.pow(k))

  def toDouble: Double = num.toDouble / den.toDouble

  def toInt: Int = toDouble.toInt

  def isWhole: Boolean = den == 1

  def ceil: Rat =
    if (num >= 0) Rat((num + den - 1) / den, 1)
    else Rat(num / den, 1)

  def floor: Rat =
    if (num >= 0) Rat(num / den, 1)
    else Rat((num - den + 1) / den, 1)

  def round: Rat =
    if (num >= 0) Rat((num + (den / 2)) / den, 1)
    else Rat((num - (den / 2)) / den, 1)

}

object Rat {

  val MinusOne: Rat = Rat(-1)
  val Zero: Rat = Rat(0)
  val One: Rat = Rat(1)
  val Two: Rat = Rat(2)

  def apply(n: BigInt): Rat =
    Rat(n, 1)

  def apply(num: BigInt, den: BigInt): Rat =
    if (den == 0) throw new ArithmeticException("/0")
    else if (den < 0) apply(-num, -den)
    else if (num == 0) new Rat(0, 1)
    else {
      val g = num.gcd(den)
      new Rat(num / g, den / g)
    }

  def unapply(r: Rat): Some[(BigInt, BigInt)] = Some((r.num, r.den))

  implicit val ratAlgebra: RatAlgebra =
    new RatAlgebra

  val RatMinMaxLattice: DistributiveLattice[Rat] =
    DistributiveLattice.minMax[Rat](using ratAlgebra)

  // Is this horrible? Yes. Am I ashamed? Yes.
  private[this] def genNonZero: Gen[BigInt] =
    arbitrary[BigInt].flatMap { x =>
      if (x != 0) Gen.const(x)
      else genNonZero
    }

  implicit val ratArbitrary: Arbitrary[Rat] =
    Arbitrary(for {
      n <- arbitrary[BigInt]
      d <- genNonZero
    } yield Rat(n, d))
}

class RatAlgebra extends Field[Rat] with Order[Rat] with Serializable {

  def compare(x: Rat, y: Rat): Int = x.compare(y)

  val zero: Rat = Rat.Zero
  val one: Rat = Rat.One

  def plus(a: Rat, b: Rat): Rat = a + b
  def negate(a: Rat): Rat = -a
  def times(a: Rat, b: Rat): Rat = a * b
  override def reciprocal(a: Rat): Rat = a.reciprocal
  def div(a: Rat, b: Rat): Rat = a / b

  override def fromInt(n: Int): Rat = Rat(n)
  override def fromBigInt(n: BigInt): Rat = Rat(n)

  def isWhole(a: Rat): Boolean = a.isWhole
  def ceil(a: Rat): Rat = a.ceil
  def floor(a: Rat): Rat = a.floor
  def round(a: Rat): Rat = a.round
}
