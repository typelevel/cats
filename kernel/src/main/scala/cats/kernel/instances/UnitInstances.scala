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
package instances
import compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
trait UnitInstances {
  implicit val catsKernelStdBoundableEnumerableForUnit: Order[Unit] with Hash[Unit] with BoundableEnumerable[Unit] with BoundedEnumerable[Unit] =
    new UnitOrder

  implicit val catsKernelStdAlgebraForUnit: BoundedSemilattice[Unit] with CommutativeGroup[Unit] =
    new UnitAlgebra

  @deprecated(message = "Please use catsKernelStdBoundableEnumerableForUnit", since = "2.10.0")
  def catsKernelStdOrderForUnit: Order[Unit] with Hash[Unit] with BoundedEnumerable[Unit] =
    catsKernelStdBoundableEnumerableForUnit
}

@deprecated(message = "Please use BoundableEnumerable.", since = "2.10.0")
trait UnitEnumerable extends BoundedEnumerable[Unit] {
  override def partialNext(x: Unit): Option[Unit] = None
  override def partialPrevious(x: Unit): Option[Unit] = None
}

private[instances] trait UnitBoundableEnumerable extends BoundableEnumerable[Unit] {
  override final val size: BigInt = BigInt(0)
  override final def fromEnum(a: Unit): BigInt = BigInt(0)
  override final def toEnumOpt(i: BigInt): Option[Unit] =
    if (i == BigInt(0)) {
      Some(())
    } else {
      None
    }
}

trait UnitBounded extends LowerBounded[Unit] with UpperBounded[Unit] {
  override def minBound: Unit = ()
  override def maxBound: Unit = ()
}

class UnitOrder extends Order[Unit] with Hash[Unit] with UnitBounded with UnitEnumerable with UnitBoundableEnumerable { self =>
  def compare(x: Unit, y: Unit): Int = 0

  def hash(x: Unit): Int = 0 // ().hashCode() == 0

  override def eqv(x: Unit, y: Unit): Boolean = true
  override def neqv(x: Unit, y: Unit): Boolean = false
  override def gt(x: Unit, y: Unit): Boolean = false
  override def lt(x: Unit, y: Unit): Boolean = false
  override def gteqv(x: Unit, y: Unit): Boolean = true
  override def lteqv(x: Unit, y: Unit): Boolean = true

  override def min(x: Unit, y: Unit): Unit = ()
  override def max(x: Unit, y: Unit): Unit = ()

  override val order: Order[Unit] = self
}

class UnitAlgebra extends BoundedSemilattice[Unit] with CommutativeGroup[Unit] {
  def empty: Unit = ()
  def combine(x: Unit, y: Unit): Unit = ()
  override def remove(x: Unit, y: Unit): Unit = ()
  def inverse(x: Unit): Unit = ()
  override protected[this] def repeatedCombineN(a: Unit, n: Int): Unit = ()
  override def combineAllOption(as: IterableOnce[Unit]): Option[Unit] =
    if (as.iterator.isEmpty) None else Some(())
}
