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
 * A meet-semilattice (or lower semilattice) is a semilattice whose
 * operation is called "meet", and which can be thought of as a
 * greatest lower bound.
 */
trait MeetSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Serializable {
  def meet(lhs: A, rhs: A): A

  def meetSemilattice: Semilattice[A] = meet(_, _)

  def meetPartialOrder(implicit ev: Eq[A]): PartialOrder[A] =
    meetSemilattice.asMeetPartialOrder
}

trait MeetSemilatticeFunctions[M[A] <: MeetSemilattice[A]] {
  def meet[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: M[A]): A =
    ev.meet(x, y)
}

object MeetSemilattice extends MeetSemilatticeFunctions[MeetSemilattice] {

  /**
   * Access an implicit `MeetSemilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: MeetSemilattice[A]): MeetSemilattice[A] = ev
}
