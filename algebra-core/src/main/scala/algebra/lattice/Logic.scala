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

import scala.{specialized => sp}

/**
 * Logic models a logic generally. It is a bounded distributive
 * lattice with an extra negation operator.
 *
 * The negation operator obeys the weak De Morgan laws:
 *  - ¬(x∨y) = ¬x∧¬y
 *  - ¬(x∧y) = ¬¬(¬x∨¬y)
 *
 * For intuitionistic logic see [[Heyting]]
 * For fuzzy logic see [[DeMorgan]]
 */
trait Logic[@sp(Int, Long) A] extends Any with BoundedDistributiveLattice[A] { self =>
  def and(a: A, b: A): A

  def or(a: A, b: A): A

  def not(a: A): A

  def xor(a: A, b: A): A = or(and(a, not(b)), and(not(a), b))
  def nand(a: A, b: A): A = not(and(a, b))
  def nor(a: A, b: A): A = not(or(a, b))
  def nxor(a: A, b: A): A = not(xor(a, b))
}

trait LogicFunctions[H[A] <: Logic[A]] {
  def complement[@sp(Int, Long) A](x: A)(implicit ev: H[A]): A =
    ev.not(x)

  def nor[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.nor(x, y)
  def nxor[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.nxor(x, y)
  def nand[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.nand(x, y)
}

object Logic extends LogicFunctions[Logic] {

  /**
   * Access an implicit `Logic[A]`.
   */
  @inline final def apply[@sp(Int, Long) A](implicit ev: Logic[A]): Logic[A] = ev

  /**
   * Turn a [[Heyting]] into a `Logic`.
   * Used for binary compatibility.
   */
  final def fromHeyting[@sp(Int, Long) A](h: Heyting[A]): Logic[A] =
    new Logic[A] {
      def and(a: A, b: A): A = h.and(a, b)

      def or(a: A, b: A): A = h.or(a, b)

      def not(a: A): A = h.complement(a)

      def zero: A = h.zero

      def one: A = h.one

      def meet(lhs: A, rhs: A): A = h.meet(lhs, rhs)

      def join(lhs: A, rhs: A): A = h.join(lhs, rhs)
    }
}
