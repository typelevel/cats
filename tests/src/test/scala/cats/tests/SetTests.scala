package cats.tests

import cats.laws.discipline.MonoidKTests

class SetTests extends CatsSuite {
  checkAll("Set[Int]", MonoidKTests[Set].monoidK[Int])
}
