package cats
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
