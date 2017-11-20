package cats.kernel.laws

import cats.kernel.Eq
import org.scalacheck.Prop

package object discipline {
  implicit def catsLawsIsEqToProp[A](isEq: IsEq[A])(implicit ev: Eq[A]): Prop =
    ev.eqv(isEq.lhs, isEq.rhs)
}
