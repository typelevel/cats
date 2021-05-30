package cats
package algebra
package instances

import cats.algebra.lattice.Bool
import cats.algebra.ring.BoolRing
import cats.algebra.ring.CommutativeRig

object boolean extends BooleanInstances

trait BooleanInstances {
  implicit val catsAlgebraStdAlgebraForBoolean: BooleanAlgebra =
    new BooleanAlgebra

  val catsAlgebraStdRingForBoolean: BoolRing[Boolean] = new BoolRing[Boolean] {
    def zero: Boolean = false
    def one: Boolean = true
    def plus(x: Boolean, y: Boolean): Boolean = x ^ y
    def times(x: Boolean, y: Boolean): Boolean = x && y
  }
}

/**
 * This commutative rig is different than the one obtained from GF(2).
 *
 * It uses || for plus, and && for times.
 */
class BooleanAlgebra extends Bool[Boolean] with CommutativeRig[Boolean] {

  def zero: Boolean = false
  def one: Boolean = true

  override def isZero(x: Boolean)(implicit ev: Eq[Boolean]): Boolean = !x
  override def isOne(x: Boolean)(implicit ev: Eq[Boolean]): Boolean = x

  def and(x: Boolean, y: Boolean): Boolean = x && y
  def or(x: Boolean, y: Boolean): Boolean = x || y
  def complement(x: Boolean): Boolean = !x

  def plus(a: Boolean, b: Boolean): Boolean = a || b
  override def pow(a: Boolean, b: Int): Boolean = a
  override def times(a: Boolean, b: Boolean): Boolean = a && b
}
