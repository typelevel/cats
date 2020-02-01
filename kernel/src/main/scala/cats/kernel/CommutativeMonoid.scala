package cats.kernel

import scala.{specialized => sp}

/**
 * CommutativeMonoid represents a commutative monoid.
 *
 * A monoid is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeMonoid[@sp(Int, Long, Float, Double) A] extends Any with Monoid[A] with CommutativeSemigroup[A]

object CommutativeMonoid extends MonoidFunctions[CommutativeMonoid] {

  /**
   * Access an implicit `CommutativeMonoid[A]`.
   */
  @inline final def apply[A](implicit ev: CommutativeMonoid[A]): CommutativeMonoid[A] = ev

  /**
   * Create a `CommutativeMonoid` instance from the given function and empty value.
   */
  @inline def instance[A](emptyValue: A, cmb: (A, A) => A): CommutativeMonoid[A] = new CommutativeMonoid[A] {
    override val empty: A = emptyValue

    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
