package cats
package tests

import cats.data.NonEmptyVector
import cats.laws.discipline.{AlternativeTests, CoflatMapTests, SerializableTests, TraverseTests, SemigroupalTests}
import cats.laws.discipline.arbitrary._

class VectorSuite extends CatsSuite {
  checkAll("Vector[Int]", SemigroupalTests[Vector].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Vector]", SerializableTests.serializable(Semigroupal[Vector]))

  checkAll("Vector[Int]", CoflatMapTests[Vector].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Vector]", SerializableTests.serializable(CoflatMap[Vector]))

  checkAll("Vector[Int]", AlternativeTests[Vector].alternative[Int, Int, Int])
  checkAll("Alternative[Vector]", SerializableTests.serializable(Alternative[Vector]))

  checkAll("Vector[Int] with Option", TraverseTests[Vector].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Vector]", SerializableTests.serializable(Traverse[Vector]))

  test("show") {
    Vector(1, 2, 3).show should === ("Vector(1, 2, 3)")

    Vector.empty[Int].show should === ("Vector()")

    forAll { vec: Vector[String] =>
      vec.show should === (vec.toString)
    }
  }

  test("nev => vector => nev returns original nev")(
    forAll { fa: NonEmptyVector[Int] =>
      assert(fa.toVector.toNev == Some(fa))
    }
  )

  test("toNev on empty vector returns None"){
    assert(Vector.empty[Int].toNev == None)
  }
}
