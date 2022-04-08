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
package lattice

import scala.{specialized => sp}

/**
 * A distributive lattice a lattice where join and meet distribute:
 *
 *   - a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)
 *   - a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)
 */
trait DistributiveLattice[@sp(Int, Long, Float, Double) A] extends Any with Lattice[A]

object DistributiveLattice
    extends JoinSemilatticeFunctions[DistributiveLattice]
    with MeetSemilatticeFunctions[DistributiveLattice] {

  /**
   * Access an implicit `Lattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit
    ev: DistributiveLattice[A]
  ): DistributiveLattice[A] = ev

  def minMax[@sp(Int, Long, Float, Double) A: Order]: DistributiveLattice[A] =
    new MinMaxLattice[A]
}

class MinMaxLattice[@sp(Int, Long, Float, Double) A](implicit order: Order[A]) extends DistributiveLattice[A] {
  def join(x: A, y: A): A = order.max(x, y)
  def meet(x: A, y: A): A = order.min(x, y)
}
