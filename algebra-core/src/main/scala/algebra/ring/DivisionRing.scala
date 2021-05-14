package algebra
package ring

import scala.{specialized => sp}

trait DivisionRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Ring[A] with MultiplicativeGroup[A] {
  self =>

  def fromDouble(a: Double): A = Field.defaultFromDouble[A](a)(self, self)

}

trait DivisionRingFunctions[F[T] <: DivisionRing[T]] extends RingFunctions[F] with MultiplicativeGroupFunctions[F] {
  def fromDouble[@sp(Int, Long, Float, Double) A](n: Double)(implicit ev: F[A]): A =
    ev.fromDouble(n)
}

object DivisionRing extends DivisionRingFunctions[DivisionRing] {

  @inline final def apply[A](implicit f: DivisionRing[A]): DivisionRing[A] = f

}
