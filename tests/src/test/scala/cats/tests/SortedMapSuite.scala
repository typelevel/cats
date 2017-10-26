package cats
package tests

import cats.kernel.laws.discipline.{HashTests, MonoidTests}
import cats.laws.discipline.{FlatMapTests, SemigroupalTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._

import scala.collection.immutable.SortedMap

class SortedMapSuite extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[SortedMap[Int, ?]]

  checkAll("SortedMap[Int, Int]", SemigroupalTests[SortedMap[Int, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[SortedMap[Int, ?]]", SerializableTests.serializable(Semigroupal[SortedMap[Int, ?]]))

  checkAll("SortedMap[Int, Int]", FlatMapTests[SortedMap[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[SortedMap[Int, ?]]", SerializableTests.serializable(FlatMap[SortedMap[Int, ?]]))

  checkAll("SortedMap[Int, Int] with Option", TraverseTests[SortedMap[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[SortedMap[Int, ?]]", SerializableTests.serializable(Traverse[SortedMap[Int, ?]]))

  test("show isn't empty and is formatted as expected") {
    forAll { (map: SortedMap[Int, String]) =>
      map.show.nonEmpty should === (true)
      map.show.startsWith("SortedMap(") should === (true)
      map.show should === (implicitly[Show[SortedMap[Int, String]]].show(map))
    }
  }

  checkAll("Hash[SortedMap[Int, String]]" , HashTests[SortedMap[Int, String]].hash)
  checkAll("Monoid[SortedMap[String, Int]]", MonoidTests[SortedMap[String, Int]].monoid)
  checkAll("Monoid[SortedMap[String, Int]]", SerializableTests.serializable(Monoid[SortedMap[String, Int]]))
}
