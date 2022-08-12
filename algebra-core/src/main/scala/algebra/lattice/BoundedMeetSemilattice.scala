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

trait BoundedMeetSemilattice[@sp(Int, Long, Float, Double) A] extends Any with MeetSemilattice[A] {
  def one: A
  def isOne(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, one)

  override def meetSemilattice: BoundedSemilattice[A] =
    new BoundedSemilattice[A] {
      def empty: A = one
      def combine(x: A, y: A): A = meet(x, y)
    }
}

trait BoundedMeetSemilatticeFunctions[B[A] <: BoundedMeetSemilattice[A]] extends MeetSemilatticeFunctions[B] {
  def one[@sp(Int, Long, Float, Double) A](implicit ev: B[A]): A =
    ev.one
}

object BoundedMeetSemilattice extends BoundedMeetSemilatticeFunctions[BoundedMeetSemilattice] {

  /**
   * Access an implicit `BoundedMeetSemilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit
    ev: BoundedMeetSemilattice[A]
  ): BoundedMeetSemilattice[A] = ev
}
