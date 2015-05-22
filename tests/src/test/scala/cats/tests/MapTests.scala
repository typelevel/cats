package cats
package tests

import cats.laws.discipline.{FlatMapTests, SerializableTests}

class MapTests extends CatsSuite {
  checkAll("FlatMap[Map[Int, Int]]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("Seriazable[FlatMap[Map[Int, ?]]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))
}
