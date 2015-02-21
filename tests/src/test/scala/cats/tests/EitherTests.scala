package cats.tests

import cats.laws.discipline.MonadTests

class EitherTests extends CatsSuite {
  checkAll("Either[Int, Int]", MonadTests[Either[Int, ?]].flatMap[Int, Int, Int])
}
