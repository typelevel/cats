package cats
package tests

import cats.laws.discipline.{TraverseTests, FlatMapTests, SerializableTests, SemigroupalTests}

class MapTests extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Map[Int, ?]]

  checkAll("Map[Int, Int]", SemigroupalTests[Map[Int, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Map[Int, ?]]", SerializableTests.serializable(Semigroupal[Map[Int, ?]]))

  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, ?]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))

  checkAll("Map[Int, Int] with Option", TraverseTests[Map[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))

  test("show isn't empty and is formatted as expected") {
    forAll { (map: Map[Int, String]) =>
      map.show.nonEmpty should === (true)
      map.show.startsWith("Map(") should === (true)
      map.show should === (implicitly[Show[Map[Int, String]]].show(map))
    }
  }
}
