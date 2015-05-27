package cats
package tests

import cats.data.Ior
import cats.laws.discipline.{MonadTests, SerializableTests}

class IorTests extends CatsSuite {
  checkAll("Ior[String, Int]", MonadTests[String Ior ?].monad[Int, Int, Int])
  checkAll("Monad[String Ior ?]]", SerializableTests.serializable(Monad[String Ior ?]))
}
