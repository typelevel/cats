package cats
package algebra
package instances

import cats.algebra.lattice.Bool
import cats.algebra.ring.BoolRing
import cats.algebra.ring.CommutativeRig

package object boolean extends BooleanInstances

trait BooleanInstances extends cats.kernel.instances.BooleanInstances {
  implicit val booleanAlgebra: BooleanAlgebra =
    new BooleanAlgebra

  val booleanRing = new BoolRing[Boolean] {
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
