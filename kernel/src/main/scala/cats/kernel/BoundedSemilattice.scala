package cats.kernel

import scala.{specialized => sp}

trait BoundedSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Semilattice[A] with CommutativeMonoid[A] {
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else a // combine(a, a) == a for a semilattice
}

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
