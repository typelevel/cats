package alleycats.tests

import alleycats.laws.discipline._
import alleycats.std.all._
import cats.Foldable
import cats.instances.all._
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.TraverseFilterTests
import cats.laws.discipline.arbitrary._

class SetSuite extends AlleycatsSuite {
  checkAll("FlatMapRec[Set]", FlatMapRecTests[Set].tailRecM[Int])

  checkAll("Foldable[Set]", SerializableTests.serializable(Foldable[Set]))

  checkAll("TraverseFilter[Set]", TraverseFilterTests[Set].traverseFilter[Int, Int, Int])
}
