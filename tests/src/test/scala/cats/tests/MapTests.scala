package cats
package tests

import cats.laws.discipline.{TraverseFilterTests, FlatMapTests, SerializableTests, CartesianTests}

class MapTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Map[Int, ?]]

  checkAll("Map[Int, Int]", CartesianTests[Map[Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Map[Int, ?]]", SerializableTests.serializable(Cartesian[Map[Int, ?]]))

  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, ?]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))

  checkAll("Map[Int, Int] with Option", TraverseFilterTests[Map[Int, ?]].traverseFilter[Int, Int, Int, Int, Option, Option])
  checkAll("TraverseFilter[Map[Int, ?]]", SerializableTests.serializable(TraverseFilter[Map[Int, ?]]))

  test("show isn't empty and is formatted as expected") {
    forAll { (map: Map[Int, String]) =>
      map.show.nonEmpty should === (true)
      map.show.startsWith("Map(") should === (true)
      map.show should === (implicitly[Show[Map[Int, String]]].show(map))
    }
  }
}
