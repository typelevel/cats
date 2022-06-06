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
 * A lattice is a set `A` together with two operations (meet and
 * join). Both operations individually constitute semilattices (join-
 * and meet-semilattices respectively): each operation is commutative,
 * associative, and idempotent.
 *
 * Join can be thought of as finding a least upper bound (supremum),
 * and meet can be thought of as finding a greatest lower bound
 * (infimum).
 *
 * The join and meet operations are also linked by absorption laws:
 *
 *   meet(a, join(a, b)) = join(a, meet(a, b)) = a
 */
trait Lattice[@sp(Int, Long, Float, Double) A] extends Any with JoinSemilattice[A] with MeetSemilattice[A] { self =>

  /**
   * This is the lattice with meet and join swapped
   */
  def dual: Lattice[A] = new Lattice[A] {
    def meet(a: A, b: A) = self.join(a, b)
    def join(a: A, b: A) = self.meet(a, b)
    override def dual = self
  }
}

object Lattice extends JoinSemilatticeFunctions[Lattice] with MeetSemilatticeFunctions[Lattice] {

  /**
   * Access an implicit `Lattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: Lattice[A]): Lattice[A] = ev
}
