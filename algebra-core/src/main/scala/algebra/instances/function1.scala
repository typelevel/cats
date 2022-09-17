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

import algebra.lattice.Logic

package object function1 extends Function1Instances

trait Function1Instances {
  implicit def function1Logic[A]: Function1Logic[A] =
    new Function1Logic[A]
}

class Function1Logic[A] extends Logic[A => Boolean] {
  def zero: A => Boolean = _ => false
  def one: A => Boolean = _ => true
  def join(lhs: A => Boolean, rhs: A => Boolean): A => Boolean = x => lhs(x) || rhs(x)
  def meet(lhs: A => Boolean, rhs: A => Boolean): A => Boolean = x => lhs(x) && rhs(x)
  def and(a: A => Boolean, b: A => Boolean): A => Boolean = meet(a, b)
  def or(a: A => Boolean, b: A => Boolean): A => Boolean = join(a, b)
  def not(a: A => Boolean): A => Boolean = x => !a(x)
}
