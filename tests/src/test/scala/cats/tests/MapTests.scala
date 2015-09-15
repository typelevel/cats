package cats
package tests

import cats.laws.discipline.{TraverseTests, FlatMapTests, SerializableTests}

class MapTests extends CatsSuite {
  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, ?]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))

  checkAll("Map[Int, Int] with Option", TraverseTests[Map[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))
}
