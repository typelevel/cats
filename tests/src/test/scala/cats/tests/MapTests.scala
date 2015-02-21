package cats.tests

import cats.laws.discipline.FlatMapTests

class MapTests extends CatsSuite {
  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
}
