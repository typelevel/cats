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
import cats.laws.discipline.MiniInt.*
import cats.laws.discipline.eq.*

trait ScalaVersionSpecificFoldableSuite
trait ScalaVersionSpecificParallelSuite
trait ScalaVersionSpecificRegressionSuite
trait ScalaVersionSpecificTraverseSuite

trait ScalaVersionSpecificAlgebraInvariantSuite {
  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  protected trait MiniIntNumeric extends Numeric[MiniInt] {
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
  }

  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = Eq.by { numeric =>
    // This allows us to catch the case where the fromInt overflows. We use the None to compare two Numeric instances,
    // verifying that when fromInt throws for one, it throws for the other.
    val fromMiniInt: MiniInt => Option[A] =
      miniInt =>
        try Some(numeric.fromInt(miniInt.toInt))
        catch {
          case _: IllegalArgumentException => None // MiniInt overflow
        }

    (
      numeric.compare _,
      numeric.plus _,
      numeric.minus _,
      numeric.times _,
      numeric.negate _,
      fromMiniInt,
      numeric.toInt _,
      numeric.toLong _,
      numeric.toFloat _,
      numeric.toDouble _
    )
  }

}
