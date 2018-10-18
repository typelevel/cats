package cats
package tests

class SplitSuite extends CatsSuite {
  test("syntax") {
    val f = ((_: Int) + 1).split((_: Int) / 2)
    f((1, 2)) should be((2, 1))
  }
}
