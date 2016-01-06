package cats
package laws

import org.scalacheck.Prop

package object discipline {
  implicit def isEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    isEq.lhs ?== isEq.rhs
}
