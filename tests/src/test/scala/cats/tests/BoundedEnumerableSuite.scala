package cats.tests

import cats.kernel.BoundedEnumerable
import cats.syntax.eq._

class BoundedEnumerableSuite extends CatsSuite {

  test("membersAscending") {
    assert(BoundedEnumerable[Boolean].membersAscending.toList === (List(false, true)))
  }

  test("membersDescending") {
    assert(BoundedEnumerable[Boolean].membersDescending.toList === (List(true, false)))
  }

  test("cycleNext") {
    assert(BoundedEnumerable[Boolean].cycleNext(false) === true)
  }

  test("cyclePrevious") {
    assert(BoundedEnumerable[Boolean].cyclePrevious(false) === true)
  }

}
