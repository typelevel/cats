package cats
package tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, SerializableTests}

class ListTests extends CatsSuite {
  checkAll("CoflatMapList[List[Int]]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("Serializable[List[Int]]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("MonadCombine[List[Int]]", MonadCombineTests[List].monadCombine[Int, Int, Int])
  checkAll("Serializable[MonadCombine[List]]", SerializableTests.serializable(MonadCombine[List]))
}
