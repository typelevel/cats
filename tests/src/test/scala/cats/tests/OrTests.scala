package cats.tests

import cats.laws.discipline.MonadTests

import cats.data.Or

class OrTests extends CatsSuite {
  checkAll("Or[String, Int]", MonadTests[String Or ?].monad[Int, Int, Int])
}
