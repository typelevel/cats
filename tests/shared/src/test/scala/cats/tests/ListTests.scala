package cats
package tests

import cats.data.Const
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class ListTests extends CatsSuite {
  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", MonadCombineTests[List].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[List]", SerializableTests.serializable(MonadCombine[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  test("traverse Const pure == id"){
    assert(
      List(1,2,3).traverseU(i => Const(List(i))).getConst == List(1,2,3)
    )
  }

  test("traverse Const Option == Some(id)"){
    assert(
      List(1,2,3).traverseU(Option(_)) == Some(List(1,2,3))
    )
  }
}
