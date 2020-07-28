package cats.tests

import cats.syntax.arrow._

class SplitSuite extends CatsSuite {
  test("syntax") {
    val f = ((_: Int) + 1).split((_: Int) / 2)
    assertEquals(f((1, 2)), (2, 1))
  }
}
