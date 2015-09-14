package cats
package tests

import cats.data.NonEmptyList
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ListTests extends CatsSuite with GeneratorDrivenPropertyChecks {
  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", MonadCombineTests[List].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[List]", SerializableTests.serializable(MonadCombine[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  test("nel => list => nel returns original nel")(
    forAll { fa: NonEmptyList[Int] =>
      assert(fa.unwrap.toNel == Some(fa))
    }
  )

  test("toNel on empty list returns None"){
    assert(List.empty[Int].toNel == None)
  }
}
