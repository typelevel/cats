package algebra
package ring

import scala.{specialized => sp}

trait Field[@sp(Int, Long, Float, Double) A]
    extends Any
    with EuclideanRing[A]
    with DivisionRing[A]
    with MultiplicativeCommutativeGroup[A] {
  self =>

  // default implementations for GCD

  override def gcd(a: A, b: A)(implicit eqA: Eq[A]): A =
    if (isZero(a) && isZero(b)) zero else one
  override def lcm(a: A, b: A)(implicit eqA: Eq[A]): A = times(a, b)

  // default implementations for Euclidean division in a field (as every nonzero element is a unit!)

  def euclideanFunction(a: A): BigInt = BigInt(0)
  def equot(a: A, b: A): A = div(a, b)
  def emod(a: A, b: A): A = zero
  override def equotmod(a: A, b: A): (A, A) = (div(a, b), zero)

  // needed for bin-compat
  override def fromDouble(a: Double): A =
    DivisionRing.defaultFromDouble[A](a)(self, self)

}

trait FieldFunctions[F[T] <: Field[T]] extends EuclideanRingFunctions[F] with MultiplicativeGroupFunctions[F] {
  def fromDouble[@sp(Int, Long, Float, Double) A](n: Double)(implicit ev: F[A]): A =
    ev.fromDouble(n)
}

object Field extends FieldFunctions[Field] {
  @inline final def apply[A](implicit ev: Field[A]): Field[A] = ev
}
