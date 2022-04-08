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
import algebra.ring.CommutativeRig

/**
 * A bounded distributive lattice is a lattice that both bounded and distributive
 */
trait BoundedDistributiveLattice[@sp(Int, Long, Float, Double) A]
    extends Any
    with BoundedLattice[A]
    with DistributiveLattice[A] { self =>

  /**
   * Return a CommutativeRig using join and meet. Note this must obey the commutative rig laws since
   * meet(a, one) = a, and meet and join are associative, commutative and distributive.
   */
  private[algebra] def asCommutativeRig: CommutativeRig[A] =
    new CommutativeRig[A] {
      def zero: A = self.zero
      def one: A = self.one
      def plus(x: A, y: A): A = self.join(x, y)
      def times(x: A, y: A): A = self.meet(x, y)
    }

  override def dual: BoundedDistributiveLattice[A] = new BoundedDistributiveLattice[A] {
    def meet(a: A, b: A) = self.join(a, b)
    def join(a: A, b: A) = self.meet(a, b)
    def one = self.zero
    def zero = self.one
    override def dual = self
  }
}

object BoundedDistributiveLattice
    extends BoundedMeetSemilatticeFunctions[BoundedDistributiveLattice]
    with BoundedJoinSemilatticeFunctions[BoundedDistributiveLattice] {

  /**
   * Access an implicit `BoundedDistributiveLattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit
    ev: BoundedDistributiveLattice[A]
  ): BoundedDistributiveLattice[A] = ev

  def minMax[@sp(Int, Long, Float, Double) A](min: A, max: A)(implicit ord: Order[A]): BoundedDistributiveLattice[A] =
    new MinMaxBoundedDistributiveLattice(min, max)
}

class MinMaxBoundedDistributiveLattice[A](min: A, max: A)(implicit o: Order[A])
    extends MinMaxLattice[A]
    with BoundedDistributiveLattice[A] {
  def zero = min
  def one = max
}
