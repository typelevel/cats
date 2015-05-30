package cats
package tests

import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class ListTests extends CatsSuite {
  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", MonadCombineTests[List].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[List]", SerializableTests.serializable(MonadCombine[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))
}
