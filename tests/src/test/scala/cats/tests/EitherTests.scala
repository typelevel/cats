package cats
package tests

import cats.laws.discipline.{TraverseTests, MonadTests, SerializableTests}

class EitherTests extends CatsSuite {
  checkAll("Either[Int, Int]", MonadTests[Either[Int, ?]].flatMap[Int, Int, Int])
  checkAll("Monad[Either[Int, ?]]", SerializableTests.serializable(Monad[Either[Int, ?]]))


  checkAll("Either[Int, Int] with Option", TraverseTests[Either[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Either[Int, ?]", SerializableTests.serializable(Traverse[Either[Int, ?]]))
}
