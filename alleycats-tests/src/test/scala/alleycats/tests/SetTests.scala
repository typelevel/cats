package alleycats.tests

import alleycats.laws.discipline._

import alleycats.std.all._

class SetsTests extends AlleycatsSuite {

  checkAll("FlatMapRec[Set]", FlatMapRecTests[Set].tailRecM[Int])

}



