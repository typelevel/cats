package cats.kernel

import scala.{specialized => sp}

/**
 * Bands are semigroups whose operation
 * (i.e. combine) is also idempotent.
 */
trait Band[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A]

object Band extends SemigroupFunctions[Band] {

  /**
   * Access an implicit `Band[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: Band[A]): Band[A] = ev
}
