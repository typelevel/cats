package cats
package algebra
package instances

import cats.algebra.ring.CommutativeRing

package object unit extends UnitInstances

trait UnitInstances extends cats.kernel.instances.UnitInstances {
  implicit val unitRing: CommutativeRing[Unit] =
    new UnitAlgebra
}

class UnitAlgebra extends CommutativeRing[Unit] {

  def zero: Unit = ()
  def one: Unit = ()

  override def isZero(x: Unit)(implicit ev: Eq[Unit]): Boolean = true
  override def isOne(x: Unit)(implicit ev: Eq[Unit]): Boolean = true

  def plus(a: Unit, b: Unit): Unit = ()
  def negate(x: Unit): Unit = ()
  def times(a: Unit, b: Unit): Unit = ()
  override def pow(a: Unit, b: Int): Unit = ()
}
