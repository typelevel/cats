package cats
package laws

import cats.kernel.laws._

import org.scalacheck.Prop

package object discipline {
  implicit def catsLawsIsEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    isEq.lhs ?== isEq.rhs

  def isEqProp[A: Eq](isEq: IsEq[A]): Prop =
    Prop(Eq[A].eqv(isEq.lhs, isEq.rhs))
}
