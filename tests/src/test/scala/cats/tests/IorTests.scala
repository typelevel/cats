package cats
package tests

import cats.data.Ior
import cats.laws.discipline.{MonadTests, SerializableTests}

class IorTests extends CatsSuite {
  checkAll("Monad[Ior[String, ?]]", MonadTests[String Ior ?].monad[Int, Int, Int])
  checkAll("Serializable[Ior[String,?]]", SerializableTests.serializable(Monad[String Ior ?]))
}
