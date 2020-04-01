package cats.tests

import cats.kernel.Group
import cats.kernel.laws.discipline.GroupTests

class GroupSuite extends CatsSuite {
  test("combine minValue") {
    Group[Int].combineN(1, Int.MinValue) should ===(Int.MinValue)
  }

  test("combine negative") {
    Group[Int].combineN(1, -1) should ===(-1)
    Group[Int].combineN(1, -10) should ===(-10)
  }

  test("companion object syntax") {
    Group[Int].inverse(1) should ===(-1)
    Group[Int].remove(1, 2) should ===(-1)
  }

  checkAll("Int", GroupTests[Int].group)
// float and double are *not* associative, and ScalaCheck knows
//  checkAll("Double", GroupLaws[Double].group)
//  checkAll("Float", GroupLaws[Float].group)
  checkAll("Long", GroupTests[Long].group)
}
