package cats
package laws

import org.scalacheck.Prop

package object discipline {
  implicit def catsLawsIsEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    cats.kernel.laws.discipline.catsLawsIsEqToProp[A](isEq)
}
