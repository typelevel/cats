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

import algebra.lattice.Bool
import algebra.ring.BoolRing
import algebra.ring.CommutativeRig

package object boolean extends BooleanInstances

trait BooleanInstances extends cats.kernel.instances.BooleanInstances {
  implicit val booleanAlgebra: BooleanAlgebra =
    new BooleanAlgebra

  val booleanRing = new BoolRing[Boolean] {
    def zero: Boolean = false
    def one: Boolean = true
    def plus(x: Boolean, y: Boolean): Boolean = x ^ y
    def times(x: Boolean, y: Boolean): Boolean = x && y
  }
}

/**
 * This commutative rig is different than the one obtained from GF(2).
 *
 * It uses || for plus, and && for times.
 */
class BooleanAlgebra extends Bool[Boolean] with CommutativeRig[Boolean] {

  def zero: Boolean = false
  def one: Boolean = true

  override def isZero(x: Boolean)(implicit ev: Eq[Boolean]): Boolean = !x
  override def isOne(x: Boolean)(implicit ev: Eq[Boolean]): Boolean = x

  def and(x: Boolean, y: Boolean): Boolean = x && y
  def or(x: Boolean, y: Boolean): Boolean = x || y
  def complement(x: Boolean): Boolean = !x

  def plus(a: Boolean, b: Boolean): Boolean = a || b
  override def pow(a: Boolean, b: Int): Boolean = a
  override def times(a: Boolean, b: Boolean): Boolean = a && b
}
