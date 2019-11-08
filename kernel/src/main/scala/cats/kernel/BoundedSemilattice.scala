package cats.kernel

import scala.{specialized => sp}

trait BoundedSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Semilattice[A] with CommutativeMonoid[A]

object BoundedSemilattice extends SemilatticeFunctions[BoundedSemilattice] {

  /**
   * Access an implicit `BoundedSemilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: BoundedSemilattice[A]): BoundedSemilattice[A] =
    ev

  /**
   * Create a `BoundedSemilattice` instance from the given function and empty value.
   */
  @inline def instance[A](emptyValue: A, cmb: (A, A) => A): BoundedSemilattice[A] = new BoundedSemilattice[A] {
    override val empty: A = emptyValue

    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
