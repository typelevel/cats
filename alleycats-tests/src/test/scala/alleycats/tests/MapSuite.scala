package alleycats.tests

import cats.laws.discipline.SerializableTests
import cats.Traverse

class MapSuite extends AlleycatsSuite {
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))
}
