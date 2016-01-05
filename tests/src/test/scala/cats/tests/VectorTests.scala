package cats
package tests

import cats.laws.discipline.{MonadCombineTests, SerializableTests, TraverseTests, MonoidalTests}
import cats.laws.discipline.eq.tuple3Eq

class VectorTests extends CatsSuite {
  checkAll("Vector[Int]", MonoidalTests[Vector].monoidal[Int, Int, Int])
  checkAll("Monoidal[Vector]", SerializableTests.serializable(Monoidal[Vector]))

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
