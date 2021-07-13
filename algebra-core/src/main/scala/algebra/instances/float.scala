package algebra
package instances

import algebra.lattice.DistributiveLattice
import algebra.ring.Field
import java.lang.Math

trait FloatInstances extends cats.kernel.instances.FloatInstances {
  implicit val floatAlgebra: Field[Float] =
    new FloatAlgebra

  // Not bounded due to the presence of NaN
  val FloatMinMaxLattice: DistributiveLattice[Float] =
    DistributiveLattice.minMax[Float]
}

/**
 * Due to the way floating-point equality works, this instance is not
 * lawful under equality, but is correct when taken as an
 * approximation of an exact value.
 *
 * If you would prefer an absolutely lawful fractional value, you'll
 * need to investigate rational numbers or more exotic types.
 */
class FloatAlgebra extends Field[Float] with Serializable {

  def zero: Float = 0.0f
  def one: Float = 1.0f

  def plus(x: Float, y: Float): Float = x + y
  def negate(x: Float): Float = -x
  override def minus(x: Float, y: Float): Float = x - y

  def times(x: Float, y: Float): Float = x * y
  def div(x: Float, y: Float): Float = x / y
  override def reciprocal(x: Float): Float = 1.0f / x

  override def pow(x: Float, y: Int): Float =
    Math.pow(x.toDouble, y.toDouble).toFloat

  override def fromInt(x: Int): Float = x.toFloat
  override def fromBigInt(n: BigInt): Float = n.toFloat
  override def fromDouble(x: Double): Float = x.toFloat
}
