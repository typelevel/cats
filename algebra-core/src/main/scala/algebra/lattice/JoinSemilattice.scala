package algebra
package lattice

import scala.{specialized => sp}

/**
 * A join-semilattice (or upper semilattice) is a semilattice whose
 * operation is called "join", and which can be thought of as a least
 * upper bound.
 */
trait JoinSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>
  def join(lhs: A, rhs: A): A

  def joinSemilattice: Semilattice[A] =
    new Semilattice[A] {
      def combine(x: A, y: A): A = self.join(x, y)
    }

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
