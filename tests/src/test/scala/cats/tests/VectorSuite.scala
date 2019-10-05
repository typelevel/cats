package cats
package tests

import cats.data.{NonEmptyVector, ZipVector}
import cats.laws.discipline.{
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class VectorSuite extends CatsSuite {
  checkAll("Vector[Int]", SemigroupalTests[Vector].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Vector]", SerializableTests.serializable(Semigroupal[Vector]))

  checkAll("Vector[Int]", CoflatMapTests[Vector].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Vector]", SerializableTests.serializable(CoflatMap[Vector]))

  checkAll("Vector[Int]", AlternativeTests[Vector].alternative[Int, Int, Int])
  checkAll("Alternative[Vector]", SerializableTests.serializable(Alternative[Vector]))

  checkAll("Vector[Int] with Option", TraverseTests[Vector].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Vector]", SerializableTests.serializable(Traverse[Vector]))

  checkAll("Vector[Int]", MonadTests[Vector].monad[Int, Int, Int])
  checkAll("Monad[Vector]", SerializableTests.serializable(Monad[Vector]))

  checkAll("Vector[Int]", TraverseFilterTests[Vector].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Vector]", SerializableTests.serializable(TraverseFilter[Vector]))

  checkAll("ZipVector[Int]", CommutativeApplyTests[ZipVector].commutativeApply[Int, Int, Int])

  test("show") {
    Vector(1, 2, 3).show should ===("Vector(1, 2, 3)")

    Vector.empty[Int].show should ===("Vector()")

    forAll { vec: Vector[String] =>
      vec.show should ===(vec.toString)
    }
  }

  test("nev => vector => nev returns original nev")(
    forAll { fa: NonEmptyVector[Int] =>
      assert(fa.toVector.toNev == Some(fa))
    }
  )

  test("toNev on empty vector returns None") {
    assert(Vector.empty[Int].toNev == None)
  }
}

final class VectorInstancesSuite extends AnyFunSuiteLike with Matchers {

  test("NonEmptyParallel instance in cats.instances.vector") {
    import cats.instances.vector._
    import cats.syntax.parallel._

    (Vector(1, 2, 3), Vector("A", "B", "C")).parTupled
  }
}
