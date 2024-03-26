/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.{Align, Alternative, CoflatMap, Eval, Monad, Semigroupal, Traverse, TraverseFilter}
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

  // TraverseFilter behaviour discriminates on the Runtime type of the Applicative
  checkAll("Vector[Int] with Option", TraverseTests[Vector].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Vector[Int] with Eval", TraverseTests[Vector].traverse[Int, Int, Int, Set[Int], Eval, Eval])
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

  test("groupByNev should be consistent with groupBy")(
    forAll { (fa: Vector[Int], f: Int => Int) =>
      assert((fa.groupByNev(f).map { case (k, v) => (k, v.toVector) }: Map[Int, Vector[Int]]) === fa.groupBy(f))
    }
  )

  test("groupByNevA should be consistent with groupByNev")(
    forAll { (fa: Vector[Int], f: Int => Int) =>
      assert(fa.groupByNevA(f.andThen(Option(_))) === Option(fa.groupByNev(f)))
    }
  )

  test("scanLeftNev should be consistent with scanLeft")(
    forAll { (fa: Vector[Int], b: Int, f: (Int, Int) => Int) =>
      assert(fa.scanLeftNev(b)(f).toVector === fa.scanLeft(b)(f))
    }
  )

  test("scanRightNev should be consistent with scanRight")(
    forAll { (fa: Vector[Int], b: Int, f: (Int, Int) => Int) =>
      assert(fa.scanRightNev(b)(f).toVector === fa.scanRight(b)(f))
    }
  )

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
