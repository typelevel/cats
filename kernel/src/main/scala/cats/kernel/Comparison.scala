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

package cats.kernel

/**
 * ADT encoding the possible results of a comparison
 */
sealed abstract class Comparison(val toInt: Int, val toDouble: Double) extends Product with Serializable

object Comparison {
  case object GreaterThan extends Comparison(1, 1.0)
  case object EqualTo extends Comparison(0, 0.0)
  case object LessThan extends Comparison(-1, -1.0)

  // Used for fromDouble
  private val SomeGt = Some(Comparison.GreaterThan)
  private val SomeEq = Some(Comparison.EqualTo)
  private val SomeLt = Some(Comparison.LessThan)

  def fromInt(int: Int): Comparison =
    if (int > 0) Comparison.GreaterThan
    else if (int == 0) Comparison.EqualTo
    else Comparison.LessThan

  def fromDouble(double: Double): Option[Comparison] =
    if (double.isNaN) None
    else if (double > 0.0) SomeGt
    else if (double == 0.0) SomeEq
    else SomeLt

  implicit val catsKernelEqForComparison: Eq[Comparison] & Monoid[Comparison] =
    new Eq[Comparison] with Monoid[Comparison] {
      def eqv(x: Comparison, y: Comparison): Boolean = x == y
      def empty: Comparison = EqualTo

      def combine(x: Comparison, y: Comparison): Comparison = x match {
        case EqualTo => y
        case comp    => comp
      }
    }
}
