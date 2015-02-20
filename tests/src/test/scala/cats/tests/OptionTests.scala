package cats.tests

import cats.laws.discipline.{MonoidKTests, MonadFilterTests}

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", MonadFilterTests[Option].monadFilter[Int, Int, Int])
  checkAll("Option[Int]", MonoidKTests[Option].monoidK[Int])
}
