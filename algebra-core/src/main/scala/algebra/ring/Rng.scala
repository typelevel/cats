package algebra
package ring

import scala.{specialized => sp}

/**
 * Rng (pronounced "Rung") consists of:
 *
 *  - a commutative group for addition (+)
 *  - a semigroup for multiplication (*)
 *
 * Alternately, a Rng can be thought of as a ring without a
 * multiplicative identity (or as a semiring with an additive
 * inverse).
 *
 * Mnemonic: "Rng is a Ring without multiplicative 'I'dentity."
 */
trait Rng[@sp(Int, Long, Float, Double) A] extends Any with Semiring[A] with AdditiveCommutativeGroup[A]

object Rng extends AdditiveGroupFunctions[Rng] with MultiplicativeSemigroupFunctions[Rng] {
  @inline final def apply[A](implicit ev: Rng[A]): Rng[A] = ev

  @inline private[algebra] def instance[A](z: A, neg: A => A, add: (A, A) => A, mul: (A, A) => A): Rng[A] =
    new Rng[A] {
      val zero: A = z
      def negate(x: A): A = neg(x)
      def plus(x: A, y: A): A = add(x, y)
      def times(x: A, y: A): A = mul(x, y)
    }
}
