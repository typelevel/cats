package cats.tests

import cats.Unfoldable

class UnfoldableTest extends CatsSuite {
  test("build creates an F[A] from a varargs") {
    Unfoldable[Vector].build(1,2,3) should === (Vector(1,2,3))
  }

  test("build creates an F[A] from a Foldable") {
    Unfoldable[Vector].fromFoldable(List(1,2,3)) should === (Vector(1,2,3))
  }
}
