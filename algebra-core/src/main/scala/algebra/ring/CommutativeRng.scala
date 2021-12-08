package algebra
package ring

import scala.{specialized => sp}

/**
 * CommutativeRng is a Rng that is commutative under multiplication.
 */
trait CommutativeRng[@sp(Int, Long, Float, Double) A] extends Any with Rng[A] with CommutativeSemiring[A]

object CommutativeRng
    extends AdditiveGroupFunctions[CommutativeRng]
    with MultiplicativeSemigroupFunctions[CommutativeRng] {
  @inline final def apply[A](implicit r: CommutativeRng[A]): CommutativeRng[A] = r
}
