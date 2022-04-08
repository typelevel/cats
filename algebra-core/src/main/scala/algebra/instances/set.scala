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

import algebra.lattice.GenBool
import algebra.ring.{BoolRng, Semiring}

package object set extends SetInstances

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit def setLattice[A]: GenBool[Set[A]] = new SetLattice[A]
  implicit def setSemiring[A]: Semiring[Set[A]] = new SetSemiring[A]

  // this instance is not compatible with setSemiring, so it is not
  // marked as implicit to avoid an ambiguity.
  def setBoolRng[A]: BoolRng[Set[A]] = new SetBoolRng[A]
}

class SetLattice[A] extends GenBool[Set[A]] {
  def zero: Set[A] = Set.empty
  def or(lhs: Set[A], rhs: Set[A]): Set[A] = lhs | rhs
  def and(lhs: Set[A], rhs: Set[A]): Set[A] = lhs & rhs
  def without(lhs: Set[A], rhs: Set[A]): Set[A] = lhs -- rhs
}

class SetSemiring[A] extends Semiring[Set[A]] {
  def zero: Set[A] = Set.empty
  def plus(x: Set[A], y: Set[A]): Set[A] = x | y
  def times(x: Set[A], y: Set[A]): Set[A] = x & y
}

class SetBoolRng[A] extends BoolRng[Set[A]] {
  def zero: Set[A] = Set.empty
  def plus(x: Set[A], y: Set[A]): Set[A] = (x -- y) | (y -- x) // this is xor
  def times(x: Set[A], y: Set[A]): Set[A] = x & y
}
