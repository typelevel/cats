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

package algebra.laws

import java.lang.{Double => JDouble, Float => JFloat}
import java.math.MathContext

import org.scalacheck.Arbitrary

import algebra.*
import algebra.ring.*

/**
 * A wrapper type for approximate floating point values like Float, Double, and
 * BigDecimal which maintains an error bound on the current approximation. The
 * `Eq` instance for this type returns true if 2 values could be equal to each
 * other, given the error bounds, rather than if they actually are equal. So,
 * if x == 0.5, and y = 0.6, and the error bound of (x - y) is greater than or
 * equal to 0.1, then it's plausible they could be equal to each other, so we
 * return true. On the other hand, if the error bound is less than 0.1, then we
 * can definitely say they cannot be equal to each other.
 */
case class FPApprox[A](approx: A, mes: A, ind: BigInt) {
  import FPApprox.{abs, Epsilon}

  private def timesWithUnderflowCheck(x: A, y: A)(implicit ev: Semiring[A], eq: Eq[A], eps: Epsilon[A]): A = {
    val z = ev.times(x, y)
    if (eps.nearZero(z) && !ev.isZero(x) && !ev.isZero(y)) eps.minValue
    else z
  }

  def unary_-(implicit ev: Rng[A]): FPApprox[A] =
    FPApprox(ev.negate(approx), mes, ind)
  def +(that: FPApprox[A])(implicit ev: Semiring[A]): FPApprox[A] =
    FPApprox(ev.plus(approx, that.approx), ev.plus(mes, that.mes), ind.max(that.ind) + 1)
  def -(that: FPApprox[A])(implicit ev: Rng[A]): FPApprox[A] =
    FPApprox(ev.minus(approx, that.approx), ev.plus(mes, that.mes), ind.max(that.ind) + 1)
  def *(that: FPApprox[A])(implicit ev: Semiring[A], eq: Eq[A], eps: Epsilon[A]): FPApprox[A] =
    FPApprox(ev.times(approx, that.approx), timesWithUnderflowCheck(mes, that.mes), ind + that.ind + 1)
  def /(that: FPApprox[A])(implicit ev: Field[A], ord: Order[A], eps: Epsilon[A]): FPApprox[A] = {
    val tmp = abs(that.approx)
    val mesApx = ev.plus(ev.div(abs(approx), tmp), ev.div(mes, that.mes))
    val mesCorrection = ev.minus(ev.div(tmp, that.mes), ev.times(ev.fromBigInt(that.ind + 1), eps.epsilon))
    val mes0 = ev.div(mesApx, mesCorrection)
    val ind0 = ind.max(that.ind + 1) + 1
    FPApprox(ev.div(approx, that.approx), mes0, ind0)
  }

  def reciprocal(implicit ev: Field[A], ord: Order[A], eps: Epsilon[A]): FPApprox[A] = {
    val tmp = abs(approx)
    val mes0 = ev.div(ev.plus(ev.div(ev.one, tmp), ev.div(ev.one, mes)), ev.minus(ev.div(tmp, mes), eps.epsilon))
    FPApprox(ev.reciprocal(approx), mes0, ind.max(1) + 1)
  }

  def pow(k: Int)(implicit ev: Field[A]): FPApprox[A] = {
    val k0 = if (k >= 0) BigInt(k) else -BigInt(k)
    FPApprox(ev.pow(approx, k), ev.pow(mes, k), (ind + 1) * k0 - 1)
  }

  def error(implicit ev: Ring[A], eps: Epsilon[A]): A =
    ev.times(ev.times(mes, ev.fromBigInt(ind)), eps.epsilon)
}

object FPApprox {
  final def abs[A](x: A)(implicit ev: Rng[A], ord: Order[A]): A =
    if (ord.lt(x, ev.zero)) ev.negate(x) else x

  def exact[A: Rng: Order](a: A): FPApprox[A] = FPApprox(a, abs(a), 0)
  def approx[A: Rng: Order](a: A): FPApprox[A] = FPApprox(a, abs(a), 1)

  trait Epsilon[A] extends Serializable {
    def minValue: A
    def epsilon: A
    def isFinite(a: A): Boolean
    def nearZero(a: A): Boolean
  }

  object Epsilon {
    def isFinite[A](a: A)(implicit eps: Epsilon[A]): Boolean = eps.isFinite(a)

    def instance[A](min: A, eps: A, isFin: A => Boolean, zero: A => Boolean): Epsilon[A] =
      new Epsilon[A] {
        def minValue: A = min
        def epsilon: A = eps
        def isFinite(a: A): Boolean = isFin(a)
        def nearZero(a: A): Boolean = zero(a)
      }

    private def isFin[A](a: A)(implicit f: A => Double): Boolean =
      !JDouble.isInfinite(f(a)) && !JDouble.isNaN(f(a))

    // These are not the actual minimums, but closest we can get without
    // causing problems.
    private val minFloat: Float = JFloat.intBitsToFloat(1 << 23)
    private val minDouble: Double = JDouble.longBitsToDouble(1L << 52)

    implicit val floatEpsilon: Epsilon[Float] =
      instance(minFloat, 1.1920929e-7f, isFin(_), x => math.abs(x) < minFloat)
    implicit val doubleEpsilon: Epsilon[Double] =
      instance(minDouble, 2.220446049250313e-16, isFin(_), x => math.abs(x) < minDouble)
    def bigDecimalEpsilon(mc: MathContext): Epsilon[BigDecimal] =
      instance(BigDecimal(1, Int.MaxValue, mc), BigDecimal(1, mc.getPrecision - 1, mc), _ => true, _ == 0)
  }

  implicit def fpApproxAlgebra[A: Field: Order: Epsilon]: FPApproxAlgebra[A] = new FPApproxAlgebra[A]

  // An Eq instance that returns true if 2 values *could* be equal.
  implicit def fpApproxEq[A: Field: Order: Epsilon]: Eq[FPApprox[A]] = (x, y) =>
    // We want to check if z +/- error contains 0
    x.approx == y.approx || {
      val z = x - y
      val err = z.error
      !Epsilon.isFinite(err) ||
      Order.lteqv(Ring[A].minus(z.approx, err), Ring[A].zero) &&
      Order.gteqv(Ring[A].plus(z.approx, err), Ring[A].zero)
    }

  implicit def arbFPApprox[A: Rng: Order: Arbitrary]: Arbitrary[FPApprox[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(FPApprox.exact[A](_)))
}

class FPApproxAlgebra[A: Order: FPApprox.Epsilon](implicit ev: Field[A]) extends Field[FPApprox[A]] with Serializable {
  def zero: FPApprox[A] = FPApprox.exact(ev.zero)
  def one: FPApprox[A] = FPApprox.exact(ev.one)

  def plus(x: FPApprox[A], y: FPApprox[A]): FPApprox[A] = x + y
  def negate(x: FPApprox[A]): FPApprox[A] = -x
  override def minus(x: FPApprox[A], y: FPApprox[A]): FPApprox[A] = x - y

  def times(x: FPApprox[A], y: FPApprox[A]): FPApprox[A] = x * y
  def div(x: FPApprox[A], y: FPApprox[A]): FPApprox[A] = x / y
  override def reciprocal(x: FPApprox[A]): FPApprox[A] = x.reciprocal // one / x
  override def pow(x: FPApprox[A], y: Int): FPApprox[A] = x.pow(y)

  override def fromInt(x: Int): FPApprox[A] = FPApprox.approx(ev.fromInt(x))
  override def fromBigInt(x: BigInt): FPApprox[A] = FPApprox.approx(ev.fromBigInt(x))
  override def fromDouble(x: Double): FPApprox[A] = FPApprox.approx(ev.fromDouble(x))
}
