package alleycats

import cats.Eq

/**
 * An `Eq[A]` that delegates to referential equality (`eq`).
 * Note that it is not referentially transparent!
 */
object ReferentialEq {
  def apply[A <: AnyRef]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A) = x eq y
  }
}
