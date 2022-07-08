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

package algebra.ring

import algebra.{CommutativeMonoid, Eq, Order}

import scala.{specialized => sp}

/**
 * A trait that expresses the existence of signs and absolute values on linearly ordered additive commutative monoids
 * (i.e. types with addition and a zero).
 *
 * The following laws holds:
 *
 * (1) if `a <= b` then `a + c <= b + c` (linear order),
 * (2) `signum(x) = -1` if `x < 0`, `signum(x) = 1` if `x > 0`, `signum(x) = 0` otherwise,
 *
 * Negative elements only appear when the scalar is taken from a additive abelian group. Then:
 *
 * (3) `abs(x) = -x` if `x < 0`, or `x` otherwise,
 *
 * Laws (1) and (2) lead to the triange inequality:
 *
 * (4) `abs(a + b) <= abs(a) + abs(b)`
 *
 * Signed should never be extended in implementations, rather the [[Signed.forAdditiveCommutativeMonoid]] and
 * [[Signed.forAdditiveCommutativeGroup subtraits]].
 *
 * It's better to have the Signed hierarchy separate from the Ring/Order hierarchy, so that
 * we do not end up with duplicate implicits.
 */
trait Signed[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any {

  def additiveCommutativeMonoid: AdditiveCommutativeMonoid[A]
  def order: Order[A]

  /**
   * Returns Zero if `a` is 0, Positive if `a` is positive, and Negative is `a` is negative.
   */
  def sign(a: A): Signed.Sign = Signed.Sign(signum(a))

  /**
   * Returns 0 if `a` is 0, 1 if `a` is positive, and -1 is `a` is negative.
   */
  def signum(a: A): Int

  /**
   * An idempotent function that ensures an object has a non-negative sign.
   */
  def abs(a: A): A

  def isSignZero(a: A): Boolean = signum(a) == 0
  def isSignPositive(a: A): Boolean = signum(a) > 0
  def isSignNegative(a: A): Boolean = signum(a) < 0

  def isSignNonZero(a: A): Boolean = signum(a) != 0
  def isSignNonPositive(a: A): Boolean = signum(a) <= 0
  def isSignNonNegative(a: A): Boolean = signum(a) >= 0
}

trait SignedFunctions[S[T] <: Signed[T]] extends cats.kernel.OrderFunctions[Order] {
  def sign[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Signed.Sign =
    ev.sign(a)
  def signum[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Int =
    ev.signum(a)
  def abs[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): A =
    ev.abs(a)
  def isSignZero[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Boolean =
    ev.isSignZero(a)
  def isSignPositive[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Boolean =
    ev.isSignPositive(a)
  def isSignNegative[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Boolean =
    ev.isSignNegative(a)
  def isSignNonZero[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Boolean =
    ev.isSignNonZero(a)
  def isSignNonPositive[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Boolean =
    ev.isSignNonPositive(a)
  def isSignNonNegative[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: S[A]): Boolean =
    ev.isSignNonNegative(a)
}

object Signed extends SignedFunctions[Signed] {

  /**
   * Signed implementation for additive commutative monoids
   */
  trait forAdditiveCommutativeMonoid[A] extends Any with Signed[A] with AdditiveCommutativeMonoid[A] {
    final override def additiveCommutativeMonoid = this
    def signum(a: A): Int = {
      val c = order.compare(a, zero)
      if (c < 0) -1
      else if (c > 0) 1
      else 0
    }
  }

  /**
   * Signed implementation for additive commutative groups
   */
  trait forAdditiveCommutativeGroup[A]
      extends Any
      with forAdditiveCommutativeMonoid[A]
      with AdditiveCommutativeGroup[A] {
    def abs(a: A): A = if (order.compare(a, zero) < 0) negate(a) else a
  }

  def apply[A](implicit s: Signed[A]): Signed[A] = s

  /**
   * A simple ADT representing the `Sign` of an object.
   */
  sealed abstract class Sign(val toInt: Int) {
    def unary_- : Sign = this match {
      case Positive => Negative
      case Negative => Positive
      case Zero     => Zero
    }

    def *(that: Sign): Sign = Sign(this.toInt * that.toInt)

    def **(that: Int): Sign = this match {
      case Positive                    => Positive
      case Zero if that == 0           => Positive
      case Zero                        => Zero
      case Negative if (that % 2) == 0 => Positive
      case Negative                    => Negative
    }
  }

  case object Zero extends Sign(0)
  case object Positive extends Sign(1)
  case object Negative extends Sign(-1)

  object Sign {
    implicit def sign2int(s: Sign): Int = s.toInt

    def apply(i: Int): Sign =
      if (i == 0) Zero else if (i > 0) Positive else Negative

    private val instance: CommutativeMonoid[Sign] with MultiplicativeCommutativeMonoid[Sign] with Eq[Sign] =
      new CommutativeMonoid[Sign] with MultiplicativeCommutativeMonoid[Sign] with Eq[Sign] {
        def eqv(x: Sign, y: Sign): Boolean = x == y
        def empty: Sign = Positive
        def combine(x: Sign, y: Sign): Sign = x * y
        def one: Sign = Positive
        def times(x: Sign, y: Sign): Sign = x * y
      }

    implicit final def signMultiplicativeMonoid: MultiplicativeCommutativeMonoid[Sign] = instance
    implicit final def signMonoid: CommutativeMonoid[Sign] = instance
    implicit final def signEq: Eq[Sign] = instance
  }

}
