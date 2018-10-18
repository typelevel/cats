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
}
