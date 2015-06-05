package cats
package tests

import cats.laws.discipline.{TraverseTests, MonadCombineTests, SerializableTests}

class VectorTests extends CatsSuite {
  checkAll("Vector[Int]", MonadCombineTests[Vector].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Vector]", SerializableTests.serializable(MonadCombine[Vector]))

  checkAll("Vector[Int] with Option", TraverseTests[Vector].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Vector]", SerializableTests.serializable(Traverse[Vector]))
}
