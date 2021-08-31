package algebra
package ring

/**
 * A Boolean rng is a rng whose multiplication is idempotent, that is
 * `a⋅a = a` for all elements ''a''. This property also implies `a+a = 0`
 * for all ''a'', and `a⋅b = b⋅a` (commutativity of multiplication).
 *
 * Every `BoolRng` is equivalent to `algebra.lattice.GenBool`.
 * See `algebra.lattice.GenBoolFromBoolRng` for details.
 */
trait BoolRng[A] extends Any with CommutativeRng[A] { self =>
  final override def negate(x: A): A = x
}

object BoolRng extends AdditiveGroupFunctions[BoolRng] with MultiplicativeSemigroupFunctions[BoolRng] {
  @inline final def apply[A](implicit r: BoolRng[A]): BoolRng[A] = r
}
