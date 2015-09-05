package cats
package tests

import cats.data.Const
import cats.laws.discipline.{MonadCombineTests, SerializableTests, TraverseTests}

class VectorTests extends CatsSuite {
  checkAll("Vector[Int]", MonadCombineTests[Vector].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Vector]", SerializableTests.serializable(MonadCombine[Vector]))

  checkAll("Vector[Int] with Option", TraverseTests[Vector].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Vector]", SerializableTests.serializable(Traverse[Vector]))

  implicit def monoidVector[A]: Monoid[Vector[A]] = MonoidK[Vector].algebra[A]

  test("traverse Const pure == id"){
    assert(
      Vector(1,2,3).traverseU(i => Const(Vector(i))).getConst == Vector(1,2,3)
    )
  }

  test("traverse Const Option == Some(id)"){
    assert(
      Vector(1,2,3).traverseU(Option(_)) == Some(Vector(1,2,3))
    )
  }
}
