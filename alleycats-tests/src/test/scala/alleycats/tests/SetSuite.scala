package alleycats.tests

import alleycats.laws.discipline._
import cats.Foldable
import cats.kernel.laws.discipline.SerializableTests

import alleycats.std.all._

class SetSuite extends AlleycatsSuite {
  checkAll("FlatMapRec[Set]", FlatMapRecTests[Set].tailRecM[Int])

  checkAll("Foldable[Set]", SerializableTests.serializable(Foldable[Set]))
}



