package alleycats.tests

import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{SerializableTests, TraverseFilterTests}
import cats.Traverse
import alleycats.std.all._

class MapSuite extends AlleycatsSuite {
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))

  checkAll("TraverseFilter[Map[Int, ?]]", TraverseFilterTests[Map[Int, ?]].traverseFilter[Int, Int, Int])
}
