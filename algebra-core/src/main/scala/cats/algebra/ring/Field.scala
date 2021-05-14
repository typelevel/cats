package cats
package algebra
package ring

import scala.{specialized => sp}

trait Field[@sp(Int, Long, Float, Double) A]
    extends Any
    with CommutativeRing[A]
    with MultiplicativeCommutativeGroup[A] { self =>

  /**
   * This is implemented in terms of basic Field ops. However, this is
   * probably significantly less efficient than can be done with a
   * specific type. So, it is recommended that this method be
   * overriden.
   *
   * This is possible because a Double is a rational number.
   */
  def fromDouble(a: Double): A = Field.defaultFromDouble(a)(self, self)

}

trait FieldFunctions[F[T] <: Field[T]] extends RingFunctions[F] with MultiplicativeGroupFunctions[F] {
  def fromDouble[@sp(Int, Long, Float, Double) A](n: Double)(implicit ev: F[A]): A =
    ev.fromDouble(n)
}

object Field extends FieldFunctions[Field] {
  @inline final def apply[A](implicit ev: Field[A]): Field[A] = ev
}
