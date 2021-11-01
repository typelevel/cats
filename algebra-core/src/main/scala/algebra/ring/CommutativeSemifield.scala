package algebra
package ring

import scala.{specialized => sp}

/**
 * CommutativeSemifield is a Semifield that is commutative under multiplication.
 */
trait CommutativeSemifield[@sp(Int, Long, Float, Double) A]
    extends Any
    with Semifield[A]
    with CommutativeRig[A]
    with MultiplicativeCommutativeGroup[A]

object CommutativeSemifield
    extends AdditiveMonoidFunctions[CommutativeSemifield]
    with MultiplicativeGroupFunctions[CommutativeSemifield] {
  @inline final def apply[A](implicit r: CommutativeSemifield[A]): CommutativeSemifield[A] = r
}
