package cats.kernel

import scala.{specialized => sp}

/**
 * CommutativeSemigroup represents a commutative semigroup.
 *
 * A semigroup is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeSemigroup[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A]

object CommutativeSemigroup extends SemigroupFunctions[CommutativeSemigroup] {

  /**
   * Access an implicit `CommutativeSemigroup[A]`.
   */
  @inline final def apply[A](implicit ev: CommutativeSemigroup[A]): CommutativeSemigroup[A] = ev

  /**
   * Create a `CommutativeSemigroup` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
