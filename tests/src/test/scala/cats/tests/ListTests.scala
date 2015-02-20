package cats.tests

import cats.laws.discipline.{MonoidKTests, MonadFilterTests}

class ListTests extends CatsSuite {
  checkAll("List[Int]", MonadFilterTests[List].monadFilter[Int, Int, Int])
  checkAll("List[Int]", MonoidKTests[List].monoidK[Int])
}
