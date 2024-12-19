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

package cats
package laws
package discipline

import cats.kernel.{BoundedSemilattice, CommutativeGroup, CommutativeMonoid}
import cats.instances.int._

/**
 * Similar to `Int`, but with a much smaller domain. The exact range of [[MiniInt]] may be tuned from time to time, so
 * consumers of this type should avoid depending on its exact range.
 *
 * `MiniInt` has integer overflow characteristics similar to `Int` (but with a smaller radix), meaning that its addition
 * and multiplication are commutative and associative.
 */
final class MiniInt private (val intBits: Int) extends AnyVal with Serializable {
  import MiniInt._

  def unary_- : MiniInt = this * negativeOne

  def toInt: Int = intBits << intShift >> intShift

  def +(o: MiniInt): MiniInt = wrapped(intBits + o.intBits)
  def *(o: MiniInt): MiniInt = wrapped(intBits * o.intBits)
  def |(o: MiniInt): MiniInt = wrapped(intBits | o.intBits)
  def /(o: MiniInt): MiniInt = wrapped(intBits / o.intBits)

  override def toString: String = s"MiniInt(toInt=$toInt, intBits=$intBits)"
}

object MiniInt {
  val bitCount: Int = 4
  val minIntValue: Int = -8
  val maxIntValue: Int = 7
  private val intShift: Int = 28
  val minValue: MiniInt = unsafeFromInt(minIntValue)
  val maxValue: MiniInt = unsafeFromInt(maxIntValue)
  val zero: MiniInt = unsafeFromInt(0)
  val one: MiniInt = unsafeFromInt(1)
  val negativeOne: MiniInt = unsafeFromInt(-1)

  def isInDomain(i: Int): Boolean = i >= minIntValue && i <= maxIntValue

  def fromInt(i: Int): Option[MiniInt] = if (isInDomain(i)) Some(unsafeFromInt(i)) else None

  def wrapped(intBits: Int): MiniInt = new MiniInt(intBits & (-1 >>> intShift))

  def unsafeFromInt(i: Int): MiniInt =
    if (isInDomain(i)) {
      new MiniInt(i << intShift >>> intShift)
    } else throw new IllegalArgumentException(s"Expected value between $minIntValue and $maxIntValue but got $i")

  val allValues: List[MiniInt] = (minIntValue to maxIntValue).map(unsafeFromInt).toList

  implicit val catsLawsEqInstancesForMiniInt: Order[MiniInt] & Hash[MiniInt] =
    new Order[MiniInt] with Hash[MiniInt] {
      def hash(x: MiniInt): Int = Hash[Int].hash(x.intBits)

      def compare(x: MiniInt, y: MiniInt): Int = Order[Int].compare(x.toInt, y.toInt)
    }

  implicit val catsLawsExhaustiveCheckForMiniInt: ExhaustiveCheck[MiniInt] =
    ExhaustiveCheck.instance(allValues)

  val miniIntAddition: CommutativeGroup[MiniInt] = new CommutativeGroup[MiniInt] {
    val empty = MiniInt.zero
    def combine(x: MiniInt, y: MiniInt): MiniInt = x + y
    def inverse(x: MiniInt): MiniInt = -x
  }

  val miniIntMultiplication: CommutativeMonoid[MiniInt] = new CommutativeMonoid[MiniInt] {
    val empty = MiniInt.one
    def combine(x: MiniInt, y: MiniInt): MiniInt = x * y
  }

  val miniIntOr: BoundedSemilattice[MiniInt] = new BoundedSemilattice[MiniInt] {
    val empty = MiniInt.zero
    def combine(x: MiniInt, y: MiniInt): MiniInt = x | y
  }
}
