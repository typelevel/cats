package algebra
package ring

import scala.{specialized => sp}

/**
 * CommutativeSemiring is a Semiring that is commutative under multiplication.
 */
trait CommutativeSemiring[@sp(Int, Long, Float, Double) A]
    extends Any
    with Semiring[A]
    with MultiplicativeCommutativeSemigroup[A]

object CommutativeSemiring
    extends AdditiveMonoidFunctions[CommutativeSemiring]
    with MultiplicativeSemigroupFunctions[CommutativeSemiring] {
  @inline final def apply[A](implicit r: CommutativeSemiring[A]): CommutativeSemiring[A] = r
}
