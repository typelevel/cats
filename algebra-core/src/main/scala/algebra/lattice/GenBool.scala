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

package algebra
package lattice

import ring.BoolRng
import scala.{specialized => sp}

/**
 * Generalized Boolean algebra, that is, a Boolean algebra without
 * the top element. Generalized Boolean algebras do not (in general)
 * have (absolute) complements, but they have ''relative complements''
 * (see [[GenBool.without]]).
 */
trait GenBool[@sp(Int, Long) A] extends Any with DistributiveLattice[A] with BoundedJoinSemilattice[A] { self =>
  def and(a: A, b: A): A
  override def meet(a: A, b: A): A = and(a, b)

  def or(a: A, b: A): A
  override def join(a: A, b: A): A = or(a, b)

  /**
   * The operation of ''relative complement'', symbolically often denoted
   * `a\b` (the symbol for set-theoretic difference, which is the
   * meaning of relative complement in the lattice of sets).
   */
  def without(a: A, b: A): A

  /**
   * Logical exclusive or, set-theoretic symmetric difference.
   * Defined as `a\b ∨ b\a`.
   */
  def xor(a: A, b: A): A = or(without(a, b), without(b, a))

  /**
   * Every generalized Boolean algebra is also a `BoolRng`, with
   * multiplication defined as `and` and addition defined as `xor`.
   */
  @deprecated("See typelevel/algebra#108 for discussion", since = "2.7.0")
  def asBoolRing: BoolRng[A] = new BoolRngFromGenBool(self)
}

/**
 * Every Boolean rng gives rise to a Boolean algebra without top:
 *  - 0 is preserved;
 *  - ring multiplication (`times`) corresponds to `and`;
 *  - ring addition (`plus`) corresponds to `xor`;
 *  - `a or b` is then defined as `a xor b xor (a and b)`;
 *  - relative complement `a\b` is defined as `a xor (a and b)`.
 *
 * `BoolRng.asBool.asBoolRing` gives back the original `BoolRng`.
 *
 * @see [[algebra.lattice.GenBool.asBoolRing]]
 */
class GenBoolFromBoolRng[A](orig: BoolRng[A]) extends GenBool[A] {
  def zero: A = orig.zero
  def and(a: A, b: A): A = orig.times(a, b)
  def or(a: A, b: A): A = orig.plus(orig.plus(a, b), orig.times(a, b))
  def without(a: A, b: A): A = orig.plus(a, orig.times(a, b))
  override def asBoolRing: BoolRng[A] = orig
}

class BoolRngFromGenBool[@sp(Int, Long) A](orig: GenBool[A]) extends BoolRng[A] {
  def zero: A = orig.zero
  def plus(x: A, y: A): A = orig.xor(x, y)
  def times(x: A, y: A): A = orig.and(x, y)
}

trait GenBoolFunctions[G[A] <: GenBool[A]] extends BoundedJoinSemilatticeFunctions[G] with MeetSemilatticeFunctions[G] {
  def and[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.and(x, y)
  def or[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.or(x, y)
  def without[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.without(x, y)
  def xor[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.xor(x, y)
}

object GenBool extends GenBoolFunctions[GenBool] {
  @inline final def apply[@sp(Int, Long) A](implicit ev: GenBool[A]): GenBool[A] = ev
}
