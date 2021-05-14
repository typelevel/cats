package cats
package algebra
package ring

import scala.{specialized => sp}

/**
 * CommutativeRing is a Ring that is commutative under multiplication.
 */
trait CommutativeRing[@sp(Int, Long, Float, Double) A]
    extends Any
    with Ring[A]
    with CommutativeRig[A]
    with CommutativeRng[A]

object CommutativeRing extends RingFunctions[CommutativeRing] {
  @inline final def apply[A](implicit r: CommutativeRing[A]): CommutativeRing[A] = r
}
