package cats
package tests

import cats.laws.discipline.{MonadCombineTests, SerializableTests}

class VectorTests extends CatsSuite {
  checkAll("MonadCombine[Vector[Int]]", MonadCombineTests[Vector].monadCombine[Int, Int, Int])
  checkAll("Serializable[MonadCombine[Vector]]", SerializableTests.serializable(MonadCombine[Vector]))
}
