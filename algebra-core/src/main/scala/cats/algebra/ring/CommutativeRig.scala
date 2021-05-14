package cats
package algebra
package ring

import scala.{specialized => sp}

/**
 * CommutativeRig is a Rig that is commutative under multiplication.
 */
trait CommutativeRig[@sp(Int, Long, Float, Double) A]
    extends Any
    with Rig[A]
    with CommutativeSemiring[A]
    with MultiplicativeCommutativeMonoid[A]

object CommutativeRig
    extends AdditiveMonoidFunctions[CommutativeRig]
    with MultiplicativeMonoidFunctions[CommutativeRig] {
  @inline final def apply[A](implicit r: CommutativeRig[A]): CommutativeRig[A] = r
}
