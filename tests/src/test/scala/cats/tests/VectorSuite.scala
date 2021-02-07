package cats.tests

import cats.{Align, Alternative, CoflatMap, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.data.{NonEmptyVector, ZipVector}
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.vector._
import cats.syntax.eq._
import org.scalacheck.Prop._

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

  checkAll("Vector[Int]", AlignTests[Vector].align[Int, Int, Int, Int])
  checkAll("Align[Vector]", SerializableTests.serializable(Align[Vector]))

  checkAll("Vector[Int]", ShortCircuitingTests[Vector].traverseFilter[Int])
  checkAll("Vector[Int]", ShortCircuitingTests[Vector].foldable[Int])

  checkAll("ZipVector[Int]", CommutativeApplyTests[ZipVector].commutativeApply[Int, Int, Int])

  test("show") {
    assert(Vector(1, 2, 3).show === "Vector(1, 2, 3)")

    assert(Vector.empty[Int].show === "Vector()")

    forAll { (vec: Vector[String]) =>
      assert(vec.show === (vec.toString))
    }
  }

  test("nev => vector => nev returns original nev")(
    forAll { (fa: NonEmptyVector[Int]) =>
      assert(fa.toVector.toNev == Some(fa))
    }
  )

  test("toNev on empty vector returns None") {
    assert(Vector.empty[Int].toNev == None)
  }

  test("traverse is stack-safe") {
    val vec = (0 until 100000).toVector
    val sumAll = Traverse[Vector]
      .traverse(vec) { i => () => i }
      .apply()
      .sum

    assert(sumAll == vec.sum)
  }
}

final class VectorInstancesSuite extends munit.FunSuite {

  test("NonEmptyParallel instance in cats.instances.vector") {
    import cats.instances.vector._
    import cats.syntax.parallel._

    (Vector(1, 2, 3), Vector("A", "B", "C")).parTupled
  }
}
