package algebra
package ring

import scala.{specialized => sp}

/**
 * Rig consists of:
 *
 *  - a commutative monoid for addition (+)
 *  - a monoid for multiplication (*)
 *
 * Alternately, a Rig can be thought of as a ring without
 * multiplicative or additive inverses (or as a semiring with a
 * multiplicative identity).
 *
 * Mnemonic: "Rig is a Ring without 'N'egation."
 */
trait Rig[@sp(Int, Long, Float, Double) A] extends Any with Semiring[A] with MultiplicativeMonoid[A]

object Rig extends AdditiveMonoidFunctions[Rig] with MultiplicativeMonoidFunctions[Rig] {
  @inline final def apply[A](implicit ev: Rig[A]): Rig[A] = ev

  @inline private[algebra] def instance[A](z: A, o: A, add: (A, A) => A, mul: (A, A) => A): Rig[A] =
    new Rig[A] {
      val zero: A = z
      val one: A = o
      def plus(x: A, y: A): A = add(x, y)
      def times(x: A, y: A): A = mul(x, y)
    }
}
