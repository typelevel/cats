package algebra
package ring

import scala.{specialized => sp}

/**
 * Semifield consists of:
 *
 *  - a commutative monoid for addition (+)
 *  - a group for multiplication (*)
 *
 * Alternately, a Semifield can be thought of as a DivisionRing without an additive inverse.
 */
trait Semifield[@sp(Int, Long, Float, Double) A] extends Any with Rig[A] with MultiplicativeGroup[A]

object Semifield extends AdditiveMonoidFunctions[Semifield] with MultiplicativeGroupFunctions[Semifield] {
  @inline final def apply[A](implicit ev: Semifield[A]): Semifield[A] = ev
}
