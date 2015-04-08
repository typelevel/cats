package cats.tests

import cats.Monad
import cats.data.Xor
import cats.laws.discipline.{MonadTests, SerializableTests}

class XorTests extends CatsSuite {
  checkAll("Xor[String, Int]", MonadTests[String Xor ?].monad[Int, Int, Int])
  checkAll("Monad[String Xor ?]]", SerializableTests.serializable(Monad[String Xor ?]))
}
