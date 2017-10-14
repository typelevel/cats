package cats
package laws

import org.scalacheck.Prop

package object discipline {

  val SerializableTests = cats.kernel.laws.discipline.SerializableTests

  implicit def catsLawsIsEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    cats.kernel.laws.discipline.catsLawsIsEqToProp[A](isEq)
}
