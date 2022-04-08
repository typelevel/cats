/*
 * Copyright (c) 2022 Typelevel
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

import algebra.ring.CommutativeRing

package object unit extends UnitInstances

trait UnitInstances extends cats.kernel.instances.UnitInstances {
  implicit val unitRing: CommutativeRing[Unit] =
    new UnitAlgebra
}

class UnitAlgebra extends CommutativeRing[Unit] {

  def zero: Unit = ()
  def one: Unit = ()

  override def isZero(x: Unit)(implicit ev: Eq[Unit]): Boolean = true
  override def isOne(x: Unit)(implicit ev: Eq[Unit]): Boolean = true

  def plus(a: Unit, b: Unit): Unit = ()
  def negate(x: Unit): Unit = ()
  def times(a: Unit, b: Unit): Unit = ()
  override def pow(a: Unit, b: Int): Unit = ()
}
