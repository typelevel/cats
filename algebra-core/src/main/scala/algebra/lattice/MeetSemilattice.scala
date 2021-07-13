package algebra
package lattice

import scala.{specialized => sp}

/**
 * A meet-semilattice (or lower semilattice) is a semilattice whose
 * operation is called "meet", and which can be thought of as a
 * greatest lower bound.
 */
trait MeetSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>
  def meet(lhs: A, rhs: A): A

  def meetSemilattice: Semilattice[A] =
    new Semilattice[A] {
      def combine(x: A, y: A): A = self.meet(x, y)
    }

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
