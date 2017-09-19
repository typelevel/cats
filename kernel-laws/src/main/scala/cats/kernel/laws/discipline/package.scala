package cats.kernel.laws

import cats.kernel.Eq
import org.scalacheck.Prop

package object discipline {
  implicit def catsLawsIsEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    isEq.lhs ?== isEq.rhs
}
