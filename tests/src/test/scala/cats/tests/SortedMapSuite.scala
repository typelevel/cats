package cats.tests

import cats.{Align, FlatMap, MonoidK, Semigroupal, Show, Traverse, TraverseFilter}
import cats.kernel.{CommutativeMonoid, Monoid}
import cats.kernel.laws.discipline.{CommutativeMonoidTests, HashTests, MonoidTests}
import cats.laws.discipline.{
  AlignTests,
  FlatMapTests,
  MonoidKTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import cats.syntax.show._

import scala.collection.immutable.SortedMap

class SortedMapSuite extends CatsSuite {
  implicit val iso: Isomorphisms[SortedMap[Int, *]] = Isomorphisms.invariant[SortedMap[Int, *]]

  checkAll("SortedMap[Int, Int]", SemigroupalTests[SortedMap[Int, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[SortedMap[Int, *]]", SerializableTests.serializable(Semigroupal[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int]", FlatMapTests[SortedMap[Int, *]].flatMap[Int, Int, Int])
  checkAll("FlatMap[SortedMap[Int, *]]", SerializableTests.serializable(FlatMap[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int] with Option",
           TraverseTests[SortedMap[Int, *]].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[SortedMap[Int, *]]", SerializableTests.serializable(Traverse[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int]", TraverseFilterTests[SortedMap[Int, *]].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[SortedMap[Int, *]]", SerializableTests.serializable(TraverseFilter[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, Int]", AlignTests[SortedMap[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[SortedMap[Int, *]]", SerializableTests.serializable(Align[SortedMap[Int, *]]))

  checkAll("SortedMap[Int, *]", ShortCircuitingTests[SortedMap[Int, *]].foldable[Int])
  checkAll("SortedMap[Int, *]", ShortCircuitingTests[SortedMap[Int, *]].traverseFilter[Int])

  test("show isn't empty and is formatted as expected") {
    forAll { (map: SortedMap[Int, String]) =>
      map.show.nonEmpty should ===(true)
      map.show.startsWith("SortedMap(") should ===(true)
      map.show should ===(implicitly[Show[SortedMap[Int, String]]].show(map))
    }
  }

  checkAll("Hash[SortedMap[Int, String]]", HashTests[SortedMap[Int, String]].hash)
  checkAll("CommutativeMonoid[SortedMap[String, Int]]",
           CommutativeMonoidTests[SortedMap[String, Int]].commutativeMonoid
  )
  checkAll("CommutativeMonoid[SortedMap[String, Int]]",
           SerializableTests.serializable(CommutativeMonoid[SortedMap[String, Int]])
  )
  checkAll("Monoid[SortedMap[String, String]]", MonoidTests[SortedMap[String, String]].monoid)
  checkAll("Monoid[SortedMap[String, String]]", SerializableTests.serializable(Monoid[SortedMap[String, String]]))

  checkAll("SortedMap[String, String]", MonoidKTests[SortedMap[String, *]].monoidK[String])
  checkAll("MonoidK[SortedMap[String, *]]", SerializableTests.serializable(MonoidK[SortedMap[String, *]]))

  test("traverse is stack-safe") {
    val items = SortedMap((0 until 100000).map { i => (i.toString, i) }: _*)
    val sumAll = Traverse[SortedMap[String, *]]
      .traverse(items) { i => () => i }
      .apply
      .values
      .sum

    assert(sumAll == items.values.sum)
  }
}
