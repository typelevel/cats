package alleycats.tests

import cats.laws.discipline.{SerializableTests, TraverseTests}
import cats.Traverse

class MapSuite extends AlleycatsSuite {
  checkAll("Map[Int, Int] with Option", TraverseTests[Map[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))
}
