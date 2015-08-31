package cats
package tests

import cats.data.Const
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class ListTests extends CatsSuite {
  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", MonadCombineTests[List].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[List]", SerializableTests.serializable(MonadCombine[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  test("traverse derive foldMap"){
    assert(
      List(1,2,3).traverseU(i => Const(List(i))).getConst == List(1,2,3).foldMap(List(_))
    )
  }
}
