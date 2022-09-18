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

import algebra.lattice.{Logic, Heyting}
//import cats.kernel.Eq

package object function1 extends Function1Instances

trait Function1Instances {
  implicit def function1Logic[A]: Function1Logic[A] =
    new Function1Logic[A]
  implicit def function1Eq[A]: Eq[A => Boolean] = Eq.instance(_ == _)
}

class Function1Logic[A] extends Logic[A => Boolean] with Heyting[A => Boolean] {
  // Type alias
  private type AB = A => Boolean

  def zero: AB = _ => false
  def one: AB = _ => true
  override def join(lhs: AB, rhs: AB): AB = x => lhs(x) || rhs(x)
  override def meet(lhs: AB, rhs: AB): AB = x => lhs(x) && rhs(x)
  def imp(a: AB, b: AB): AB = or(not(a), b)
  def and(a: AB, b: AB): AB = meet(a, b)
  def or(a: AB, b: AB): AB = join(a, b)
  def not(a: AB): AB = x => !a(x)
  def complement(a: AB): AB = not(a)
  override def xor(a: AB, b: AB): AB = or(and(a, not(b)), and(not(a), b))
  override def nand(a: AB, b: AB): AB = not(and(a, b))
  override def nor(a: AB, b: AB): AB = not(or(a, b))
  override def nxor(a: AB, b: AB): AB = not(xor(a, b))
}
