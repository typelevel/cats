package algebra
package ring

/**
 * A Boolean ring is a ring whose multiplication is idempotent, that is
 * `a⋅a = a` for all elements ''a''. This property also implies `a+a = 0`
 * for all ''a'', and `a⋅b = b⋅a` (commutativity of multiplication).
 *
 * Every Boolean ring is equivalent to a Boolean algebra.
 * See `algebra.lattice.BoolFromBoolRing` for details.
 */
trait BoolRing[A] extends Any with BoolRng[A] with CommutativeRing[A]

object BoolRing extends RingFunctions[BoolRing] {
  @inline final def apply[A](implicit r: BoolRing[A]): BoolRing[A] = r
}
