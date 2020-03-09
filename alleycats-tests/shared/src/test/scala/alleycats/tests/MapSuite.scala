package alleycats.tests

import cats.Traverse
import cats.instances.all._
import cats.laws.discipline.{SerializableTests, TraverseFilterTests}
import cats.laws.discipline.arbitrary._

class MapSuite extends AlleycatsSuite {
  checkAll("Traverse[Map[Int, *]]", SerializableTests.serializable(Traverse[Map[Int, *]]))

  checkAll("TraverseFilter[Map[Int, *]]", TraverseFilterTests[Map[Int, *]].traverseFilter[Int, Int, Int])
}
