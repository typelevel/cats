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

package cats.tests

import cats.kernel.{Eq, Order}
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.laws.discipline.MiniInt._
import cats.laws.discipline.MiniFloat
import cats.laws.discipline.eq._

trait ScalaVersionSpecificFoldableSuite
trait ScalaVersionSpecificParallelSuite
trait ScalaVersionSpecificRegressionSuite
trait ScalaVersionSpecificTraverseSuite

trait ScalaVersionSpecificAlgebraInvariantSuite {
  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  protected val integralForMiniInt: Integral[MiniInt] = new Integral[MiniInt] {
    def compare(x: MiniInt, y: MiniInt): Int = Order[MiniInt].compare(x, y)
    def plus(x: MiniInt, y: MiniInt): MiniInt = x + y
    def minus(x: MiniInt, y: MiniInt): MiniInt = x + (-y)
    def times(x: MiniInt, y: MiniInt): MiniInt = x * y
    def negate(x: MiniInt): MiniInt = -x
    def fromInt(x: Int): MiniInt = MiniInt.unsafeFromInt(x)
    def toInt(x: MiniInt): Int = x.toInt
    def toLong(x: MiniInt): Long = x.toInt.toLong
    def toFloat(x: MiniInt): Float = x.toInt.toFloat
    def toDouble(x: MiniInt): Double = x.toInt.toDouble
    def quot(x: MiniInt, y: MiniInt): MiniInt = MiniInt.unsafeFromInt(x.toInt / y.toInt)
    def rem(x: MiniInt, y: MiniInt): MiniInt = MiniInt.unsafeFromInt(x.toInt % y.toInt)
  }

  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  protected val fractionalForMiniFloat: Fractional[MiniFloat] = new Fractional[MiniFloat] {
    def compare(x: MiniFloat, y: MiniFloat): Int = Order[MiniFloat].compare(x, y)
    def plus(x: MiniFloat, y: MiniFloat): MiniFloat = x + y
    def minus(x: MiniFloat, y: MiniFloat): MiniFloat = x + (-y)
    def times(x: MiniFloat, y: MiniFloat): MiniFloat = x * y
    def div(x: MiniFloat, y: MiniFloat): MiniFloat = x / y
    def negate(x: MiniFloat): MiniFloat = -x
    def fromInt(x: Int): MiniFloat = MiniFloat.from(x)
    def toInt(x: MiniFloat): Int = x.toInt
    def toLong(x: MiniFloat): Long = x.toInt.toLong
    def toFloat(x: MiniFloat): Float = x.toInt.toFloat
    def toDouble(x: MiniFloat): Double = x.toInt.toDouble
  }

  /**
   * Emulates the behaviour of `Numeric#fromInt`, but using MiniInt as the input. This allows us to exercise the
   * implementation of `fromInt` for an instance of `Numeric` while still taking advantage of the `ExhaustiveCheck`
   * instance for `MiniInt`.
   *
   * Note that this will return `None` when `fromInt` overflows. We can use this to compare two `Numeric` instances,
   * verifying that when `fromInt` throws for one, it throws for the other.
   */
  private def numericFromMiniInt[A](miniInt: MiniInt, numeric: Numeric[A]): Option[A] =
    try Some(numeric.fromInt(miniInt.toInt))
    catch {
      case _: IllegalArgumentException => None // MiniInt overflow
    }

  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = Eq.by { numeric =>
    (
      numeric.compare _,
      numeric.plus _,
      numeric.minus _,
      numeric.times _,
      numeric.negate _,
      numericFromMiniInt[A](_, numeric),
      numeric.toInt _,
      numeric.toLong _,
      numeric.toFloat _,
      numeric.toDouble _
    )
  }

  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  implicit protected def eqFractional[A: Eq: ExhaustiveCheck]: Eq[Fractional[A]] = {
    Eq.by { fractional =>
      (
        fractional.compare _,
        fractional.plus _,
        fractional.minus _,
        fractional.times _,
        fractional.negate _,
        numericFromMiniInt[A](_, fractional),
        fractional.toInt _,
        fractional.toLong _,
        fractional.toFloat _,
        fractional.toDouble _
      )
    }
  }
}
