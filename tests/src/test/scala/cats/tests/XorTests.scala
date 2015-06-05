package cats
package tests

import cats.data.Xor
import cats.laws.discipline.{TraverseTests, MonadTests, SerializableTests}

class XorTests extends CatsSuite {
  checkAll("Xor[String, Int]", MonadTests[String Xor ?].monad[Int, Int, Int])
  checkAll("Monad[String Xor ?]", SerializableTests.serializable(Monad[String Xor ?]))

  checkAll("Xor[String, Int] with Option", TraverseTests[Xor[String,?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Xor[String,?]]", SerializableTests.serializable(Traverse[Xor[String,?]]))

}
