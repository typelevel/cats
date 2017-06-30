package cats
package tests

import cats.kernel.laws.GroupLaws

class GroupTests extends CatsSuite {
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

  checkAll("Int", GroupLaws[Int].group)
  checkAll("Double", GroupLaws[Double].group)
// float is *not* associative, and scalacheck knows
//  checkAll("Float", GroupLaws[Float].group)
  checkAll("Long", GroupLaws[Long].group)
}
