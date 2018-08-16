package cats
package tests


import cats.laws.discipline.{FlatMapTests, FunctorEmptyTests, SemigroupalTests, SerializableTests, UnorderedTraverseTests, ComposeTests}
import cats.laws.discipline.arbitrary._
import cats.arrow.Compose

class MapSuite extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Map[Int, ?]]

  checkAll("Map[Int, Int]", SemigroupalTests[Map[Int, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Map[Int, ?]]", SerializableTests.serializable(Semigroupal[Map[Int, ?]]))

  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, ?]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))

  checkAll("Map[Int, Int] with Option", UnorderedTraverseTests[Map[Int, ?]].unorderedTraverse[Int, Int, Int, Option, Option])
  checkAll("UnorderedTraverse[Map[Int, ?]]", SerializableTests.serializable(UnorderedTraverse[Map[Int, ?]]))

  checkAll("Map[Int, Int]", FunctorEmptyTests[Map[Int, ?]].functorEmpty[Int, Int, Int])
  checkAll("FunctorEmpty[Map[Int, ?]]", SerializableTests.serializable(FunctorEmpty[Map[Int, ?]]))

  checkAll("Map[Int, Long]", ComposeTests[Map].compose[Int, Long, String, Double])
  checkAll("Compose[Map]", SerializableTests.serializable(Compose[Map]))



  test("show isn't empty and is formatted as expected") {
    forAll { (map: Map[Int, String]) =>
      map.show.nonEmpty should === (true)
      map.show.startsWith("Map(") should === (true)
      map.show should === (implicitly[Show[Map[Int, String]]].show(map))
    }
  }
}
