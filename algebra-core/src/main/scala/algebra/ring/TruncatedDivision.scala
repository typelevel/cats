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

import scala.{specialized => sp}

/**
 * Division and modulus for computer scientists
 * taken from https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
 *
 * For two numbers x (dividend) and y (divisor) on an ordered ring with y != 0,
 * there exists a pair of numbers q (quotient) and r (remainder)
 * such that these laws are satisfied:
 *
 * (1) q is an integer
 * (2) x = y * q + r (division rule)
 * (3) |r| < |y|,
 * (4t) r = 0 or sign(r) = sign(x),
 * (4f) r = 0 or sign(r) = sign(y).
 *
 * where sign is the sign function, and the absolute value
 * function |x| is defined as |x| = x if x >=0, and |x| = -x otherwise.
 *
 * We define functions tmod and tquot such that:
 * q = tquot(x, y) and r = tmod(x, y) obey rule (4t),
 * (which truncates effectively towards zero)
 * and functions fmod and fquot such that:
 * q = fquot(x, y) and r = fmod(x, y) obey rule (4f)
 * (which floors the quotient and effectively rounds towards negative infinity).
 *
 * Law (4t) corresponds to ISO C99 and Haskell's quot/rem.
 * Law (4f) is described by Knuth and used by Haskell,
 * and fmod corresponds to the REM function of the IEEE floating-point standard.
 */
trait TruncatedDivision[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Signed[A] {
  def tquot(x: A, y: A): A
  def tmod(x: A, y: A): A
  def tquotmod(x: A, y: A): (A, A) = (tquot(x, y), tmod(x, y))

  def fquot(x: A, y: A): A
  def fmod(x: A, y: A): A
  def fquotmod(x: A, y: A): (A, A) = (fquot(x, y), fmod(x, y))
}

trait TruncatedDivisionFunctions[S[T] <: TruncatedDivision[T]] extends SignedFunctions[S] {
  def tquot[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: TruncatedDivision[A]): A =
    ev.tquot(x, y)
  def tmod[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: TruncatedDivision[A]): A =
    ev.tmod(x, y)
  def tquotmod[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: TruncatedDivision[A]): (A, A) =
    ev.tquotmod(x, y)
  def fquot[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: TruncatedDivision[A]): A =
    ev.fquot(x, y)
  def fmod[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: TruncatedDivision[A]): A =
    ev.fmod(x, y)
  def fquotmod[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: TruncatedDivision[A]): (A, A) =
    ev.fquotmod(x, y)
}

object TruncatedDivision extends TruncatedDivisionFunctions[TruncatedDivision] {
  trait forCommutativeRing[@sp(Byte, Short, Int, Long, Float, Double) A]
      extends Any
      with TruncatedDivision[A]
      with Signed.forAdditiveCommutativeGroup[A]
      with CommutativeRing[A] { self =>

    def fmod(x: A, y: A): A = {
      val tm = tmod(x, y)
      if (signum(tm) == -signum(y)) plus(tm, y) else tm
    }

    def fquot(x: A, y: A): A = {
      val (tq, tm) = tquotmod(x, y)
      if (signum(tm) == -signum(y)) minus(tq, one) else tq
    }

    override def fquotmod(x: A, y: A): (A, A) = {
      val (tq, tm) = tquotmod(x, y)
      val signsDiffer = signum(tm) == -signum(y)
      val fq = if (signsDiffer) minus(tq, one) else tq
      val fm = if (signsDiffer) plus(tm, y) else tm
      (fq, fm)
    }

  }

  def apply[A](implicit ev: TruncatedDivision[A]): TruncatedDivision[A] = ev
}
