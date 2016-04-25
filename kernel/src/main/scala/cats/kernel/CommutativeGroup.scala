package cats.kernel

import scala.{ specialized => sp }

/**
 * An abelian group is a group whose operation is commutative.
 */
trait CommutativeGroup[@sp(Int, Long, Float, Double) A] extends Any with Group[A] with CommutativeMonoid[A]

object CommutativeGroup extends GroupFunctions[CommutativeGroup] {

  /**
   * Access an implicit `CommutativeGroup[A]`.
   */
  @inline final def apply[A](implicit ev: CommutativeGroup[A]): CommutativeGroup[A] = ev
}

