package algebra
package lattice

import scala.{specialized => sp}

/**
 * A bounded lattice is a lattice that additionally has one element
 * that is the bottom (zero, also written as ⊥), and one element that
 * is the top (one, also written as ⊤).
 *
 * This means that for any a in A:
 *
 *   join(zero, a) = a = meet(one, a)
 *
 * Or written using traditional notation:
 *
 *   (0 ∨ a) = a = (1 ∧ a)
 */
trait BoundedLattice[@sp(Int, Long, Float, Double) A]
    extends Any
    with Lattice[A]
    with BoundedMeetSemilattice[A]
    with BoundedJoinSemilattice[A] { self =>
  override def dual: BoundedLattice[A] = new BoundedLattice[A] {
    def meet(a: A, b: A) = self.join(a, b)
    def join(a: A, b: A) = self.meet(a, b)
    def one = self.zero
    def zero = self.one
    override def dual = self
  }
}

object BoundedLattice
    extends BoundedMeetSemilatticeFunctions[BoundedLattice]
    with BoundedJoinSemilatticeFunctions[BoundedLattice] {

  /**
   * Access an implicit `BoundedLattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: BoundedLattice[A]): BoundedLattice[A] = ev
}
