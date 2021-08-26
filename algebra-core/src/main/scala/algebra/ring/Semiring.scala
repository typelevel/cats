package algebra
package ring

import scala.{specialized => sp}

/**
 * Semiring consists of:
 *
 *  - a commutative monoid for addition (+)
 *  - a semigroup for multiplication (*)
 *
 * Alternately, a Semiring can be thought of as a ring without a
 * multiplicative identity or an additive inverse.
 *
 * A Semiring with an additive inverse (-) is a Rng.
 * A Semiring with a multiplicative identity (1) is a Rig.
 * A Semiring with both of those is a Ring.
 */
trait Semiring[@sp(Int, Long, Float, Double) A]
    extends Any
    with AdditiveCommutativeMonoid[A]
    with MultiplicativeSemigroup[A]

object Semiring extends AdditiveMonoidFunctions[Semiring] with MultiplicativeSemigroupFunctions[Semiring] {
  @inline final def apply[A](implicit ev: Semiring[A]): Semiring[A] = ev

  @inline private[algebra] def instance[A](z: A, add: (A, A) => A, mul: (A, A) => A): Semiring[A] =
    new Semiring[A] {
      val zero: A = z
      def plus(x: A, y: A): A = add(x, y)
      def times(x: A, y: A): A = mul(x, y)
    }
}
