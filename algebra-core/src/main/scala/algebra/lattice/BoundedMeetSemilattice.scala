package algebra
package lattice

import scala.{specialized => sp}

trait BoundedMeetSemilattice[@sp(Int, Long, Float, Double) A] extends Any with MeetSemilattice[A] { self =>
  def one: A
  def isOne(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, one)

  override def meetSemilattice: BoundedSemilattice[A] =
    new BoundedSemilattice[A] {
      def empty: A = self.one
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
