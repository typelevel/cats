package cats
package tests

import cats.laws.discipline.{MonadTests, SerializableTests}

class EitherTests extends CatsSuite {
  checkAll("Either[Int, Int]", MonadTests[Either[Int, ?]].flatMap[Int, Int, Int])
  checkAll("Monad[Either[Int, ?]]", SerializableTests.serializable(Monad[Either[Int, ?]]))
}
