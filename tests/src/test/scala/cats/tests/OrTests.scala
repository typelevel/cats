package cats.tests

import cats.Monad
import cats.data.Or
import cats.laws.discipline.{MonadTests, SerializableTests}

class OrTests extends CatsSuite {
  checkAll("Or[String, Int]", MonadTests[String Or ?].monad[Int, Int, Int])
  checkAll("Monad[String Or ?]]", SerializableTests.serializable(Monad[String Or ?]))
}
