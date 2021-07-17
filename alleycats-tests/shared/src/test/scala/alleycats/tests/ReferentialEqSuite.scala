package alleycats.tests

import alleycats.ReferentialEq
import cats.kernel.Eq
import cats.kernel.laws.discipline._

class ReferentialEqSuite extends AlleycatsSuite {
  implicit val eqObject: Eq[Object] = ReferentialEq[Object]

  checkAll("ReferentialEq[Object]", EqTests[Object].eqv)
}
