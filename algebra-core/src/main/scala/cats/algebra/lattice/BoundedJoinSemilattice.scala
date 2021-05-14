package cats
package algebra
package lattice

import scala.{specialized => sp}

trait BoundedJoinSemilattice[@sp(Int, Long, Float, Double) A] extends Any with JoinSemilattice[A] { self =>
  def zero: A
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)

  override def joinSemilattice: BoundedSemilattice[A] =
    new BoundedSemilattice[A] {
      def empty: A = self.zero
      def combine(x: A, y: A): A = join(x, y)
    }
}

trait BoundedJoinSemilatticeFunctions[B[A] <: BoundedJoinSemilattice[A]] extends JoinSemilatticeFunctions[B] {
  def zero[@sp(Int, Long, Float, Double) A](implicit ev: B[A]): A = ev.zero
}

object BoundedJoinSemilattice
    extends JoinSemilatticeFunctions[BoundedJoinSemilattice]
    with BoundedJoinSemilatticeFunctions[BoundedJoinSemilattice] {

  /**
   * Access an implicit `BoundedJoinSemilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit
    ev: BoundedJoinSemilattice[A]
  ): BoundedJoinSemilattice[A] = ev
}
