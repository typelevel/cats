package cats.tests

import cats.laws.discipline.MonadCombineTests

class VectorTests extends CatsSuite {
  checkAll("Vector[Int]", MonadCombineTests[Vector].monadCombine[Int, Int, Int])
}
