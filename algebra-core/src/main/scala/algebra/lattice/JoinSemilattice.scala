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
 * A join-semilattice (or upper semilattice) is a semilattice whose
 * operation is called "join", and which can be thought of as a least
 * upper bound.
 */
trait JoinSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Serializable {
  def join(lhs: A, rhs: A): A

  def joinSemilattice: Semilattice[A] = join(_, _)

  def joinPartialOrder(implicit ev: Eq[A]): PartialOrder[A] =
    joinSemilattice.asJoinPartialOrder
}

trait JoinSemilatticeFunctions[J[A] <: JoinSemilattice[A]] {
  def join[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: J[A]): A =
    ev.join(x, y)
}

object JoinSemilattice extends JoinSemilatticeFunctions[JoinSemilattice] {

  /**
   * Access an implicit `JoinSemilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: JoinSemilattice[A]): JoinSemilattice[A] = ev

}
