package alleycats.tests

import alleycats.ReferentialEq
import alleycats.laws.discipline.ReferentialEqTests
import cats.kernel.Eq

class ReferentialEqSuite extends AlleycatsSuite {
  implicit val eqObject: Eq[Object] = ReferentialEq[Object]

  checkAll("ReferentialEq[Object]", ReferentialEqTests[Object].eqv)
}
