package alleycats.tests

import cats.Traverse
import cats.instances.all._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{SerializableTests, ShortCircuitingTests, TraverseFilterTests}

class MapSuite extends AlleycatsSuite {
  checkAll("Traverse[Map[Int, *]]", SerializableTests.serializable(Traverse[Map[Int, *]]))

  checkAll("TraverseFilter[Map[Int, *]]", TraverseFilterTests[Map[Int, *]].traverseFilter[Int, Int, Int])

  checkAll("Map[Int, *]", ShortCircuitingTests[Map[Int, *]].traverseFilter[Int])

  test("traverse is stack-safe") {
    val items = Map((0 until 100000).map { i => (i.toString, i) }: _*)
    val sumAll = Traverse[Map[String, *]]
      .traverse(items) { i => () => i }
      .apply()
      .values
      .sum

    assert(sumAll == items.values.sum)
  }
}
