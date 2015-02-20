package cats.tests

import cats.laws.discipline.MonoidKTests

class VectorTests extends CatsSuite {
  checkAll("Vector[Int]", MonoidKTests[Vector].monoidK[Int])
}
