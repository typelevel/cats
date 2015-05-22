package cats
package tests

import cats.data.Xor
import cats.laws.discipline.{MonadTests, SerializableTests}

class XorTests extends CatsSuite {
  checkAll("Monad[Xor[String, Int]]", MonadTests[String Xor ?].monad[Int, Int, Int])
  checkAll("Serializable[Monad[String Xor ?]]", SerializableTests.serializable(Monad[String Xor ?]))
}
