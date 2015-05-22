package cats
package tests

import cats.laws.discipline.{MonadTests, SerializableTests}

class EitherTests extends CatsSuite {
  checkAll("Monad[Either[Int, Int]]", MonadTests[Either[Int, ?]].flatMap[Int, Int, Int])
  checkAll("Serializable[Either[Int, ?]]", SerializableTests.serializable(Monad[Either[Int, ?]]))
}
