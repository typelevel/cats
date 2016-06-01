package cats
package tests

import cats.laws.discipline.{MonadCombineTests, CoflatMapTests, SerializableTests, TraverseTests, CartesianTests}

class VectorTests extends CatsSuite {
  checkAll("Vector[Int]", CartesianTests[Vector].cartesian[Int, Int, Int])
  checkAll("Cartesian[Vector]", SerializableTests.serializable(Cartesian[Vector]))

  checkAll("Vector[Int]", CoflatMapTests[Vector].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Vector]", SerializableTests.serializable(CoflatMap[Vector]))

  checkAll("Vector[Int]", MonadCombineTests[Vector].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Vector]", SerializableTests.serializable(MonadCombine[Vector]))

  checkAll("Vector[Int] with Option", TraverseTests[Vector].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Vector]", SerializableTests.serializable(Traverse[Vector]))

  test("show") {
    Vector(1, 2, 3).show should === ("Vector(1, 2, 3)")

    Vector.empty[Int].show should === ("Vector()")

    forAll { vec: Vector[String] =>
      vec.show should === (vec.toString)
    }
  }

}
