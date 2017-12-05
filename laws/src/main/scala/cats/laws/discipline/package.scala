package cats
package laws

import org.scalacheck.Prop
import org.scalacheck.util.Pretty

package object discipline {

  val SerializableTests = cats.kernel.laws.discipline.SerializableTests

  implicit def catsLawsIsEqToProp[A: Eq](isEq: IsEq[A])(implicit pp: A => Pretty): Prop =
    cats.kernel.laws.discipline.catsLawsIsEqToProp[A](isEq)
}
