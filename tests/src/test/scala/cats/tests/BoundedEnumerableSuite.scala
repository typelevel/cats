package cats.tests

import cats.kernel.BoundedEnumerable

class BoundedEnumerableSuite extends CatsSuite {

  test("membersAscending") {
    BoundedEnumerable[Boolean].membersAscending.toList should ===(List(false, true))
  }

  test("membersDescending") {
    BoundedEnumerable[Boolean].membersDescending.toList should ===(List(true, false))
  }

  test("cycleNext") {
    BoundedEnumerable[Boolean].cycleNext(false) should ===(true)
  }

  test("cyclePrevious") {
    BoundedEnumerable[Boolean].cyclePrevious(false) should ===(true)
  }

}
