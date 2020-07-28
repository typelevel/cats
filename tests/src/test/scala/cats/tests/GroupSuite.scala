package cats.tests

import cats.kernel.Group
import cats.kernel.laws.discipline.GroupTests
import cats.syntax.eq._

class GroupSuite extends CatsSuite {
  test("combine minValue") {
    assert(Group[Int].combineN(1, Int.MinValue) === (Int.MinValue))
  }

  test("combine negative") {
    assert(Group[Int].combineN(1, -1) === (-1))
    assert(Group[Int].combineN(1, -10) === (-10))
  }

  test("companion object syntax") {
    assert(Group[Int].inverse(1) === (-1))
    assert(Group[Int].remove(1, 2) === (-1))
  }

  checkAll("Int", GroupTests[Int].group)
// float and double are *not* associative, and ScalaCheck knows
//  checkAll("Double", GroupLaws[Double].group)
//  checkAll("Float", GroupLaws[Float].group)
  checkAll("Long", GroupTests[Long].group)
}
