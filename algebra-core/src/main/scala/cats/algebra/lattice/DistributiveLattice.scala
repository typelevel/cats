package cats
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
