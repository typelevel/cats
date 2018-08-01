package cats.kernel

import scala.{specialized => sp}

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@sp(Int, Long, Float, Double) A] extends Any with Magma[A] {

  // In a semigroup the combine operation is associate

}

abstract class SemigroupFunctions[S[T] <: Semigroup[T]] extends MagmaFunctions[S]

object Semigroup extends SemigroupFunctions[Semigroup] {

  /**
   * Access an implicit `Semigroup[A]`.
   */
  @inline final def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev

  /**
   * Create a `Semigroup` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
