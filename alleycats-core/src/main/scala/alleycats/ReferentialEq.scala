package alleycats

import cats.Eq

/**
 * An `Eq[A]` that delegates to referential equality (`eq`).
 * Note that it is not referentially transparent!
 */
class ReferentialEq[A <: AnyRef] extends Eq[A] {
  def eqv(x: A, y: A) = x eq y
}

object ReferentialEq {
  private[this] val referentialEq: Eq[AnyRef] = new ReferentialEq[AnyRef]

  def apply[A <: AnyRef]: Eq[A] = referentialEq.asInstanceOf[Eq[A]]
}
