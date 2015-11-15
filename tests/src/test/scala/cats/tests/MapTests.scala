package cats
package tests

import cats.laws.discipline.{TraverseTests, FlatMapTests, SerializableTests}

class MapTests extends CatsSuite {
  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, ?]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))

  checkAll("Map[Int, Int] with Option", TraverseTests[Map[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))

  test("show isn't empty and is formatted as expected") {
    val mapShow = implicitly[Show[Map[Int, String]]]

    forAll { (map: Map[Int, String]) =>
      val show = mapShow.show(map)
      show.nonEmpty should === (true)
      show.startsWith("Map(") should === (true)
    }
  }
}
