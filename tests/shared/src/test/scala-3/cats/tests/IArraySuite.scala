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
// import cats.data.{NonEmptyIArray, ZipIArray}
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary.*
import cats.syntax.show.*
import cats.syntax.eq.*
import cats.instances.iarray.*
import org.scalacheck.Prop.*

class IArraySuite extends CatsSuite {
  checkAll("IArray[Int]", SemigroupalTests[IArray].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[IArray]", SerializableTests.serializable(Semigroupal[IArray]))

  checkAll("IArray[Int]", CoflatMapTests[IArray].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[IArray]", SerializableTests.serializable(CoflatMap[IArray]))

  checkAll("IArray[Int]", AlternativeTests[IArray].alternative[Int, Int, Int])
  checkAll("Alternative[IArray]", SerializableTests.serializable(Alternative[IArray]))

  // TraverseFilter behaviour discriminates on the Runtime type of the Applicative
  checkAll("IArray[Int] with Option", TraverseTests[IArray].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("IArray[Int] with Eval", TraverseTests[IArray].traverse[Int, Int, Int, Set[Int], Eval, Eval])
  checkAll("Traverse[IArray]", SerializableTests.serializable(Traverse[IArray]))

  checkAll("IArray[Int]", MonadTests[IArray].monad[Int, Int, Int])
  checkAll("Monad[IArray]", SerializableTests.serializable(Monad[IArray]))

  checkAll("IArray[Int]", TraverseFilterTests[IArray].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[IArray]", SerializableTests.serializable(TraverseFilter[IArray]))

  checkAll("IArray[Int]", AlignTests[IArray].align[Int, Int, Int, Int])
  checkAll("Align[IArray]", SerializableTests.serializable(Align[IArray]))

  checkAll("IArray[Int]", ShortCircuitingTests[IArray].traverseFilter[Int])
  checkAll("IArray[Int]", ShortCircuitingTests[IArray].foldable[Int])

  test("show") {
    assert(IArray(1, 2, 3).show === "IArray(1, 2, 3)")

    assert(IArray.empty[Int].show === "IArray()")

    forAll { (vec: IArray[String]) =>
      assert(vec.show === vec.mkString("IArray(", ", ", ")"))
    }
  }

  test("traverse is stack-safe") {
    val vec = IArray.range(0, 1000000)
    val sumAll = Traverse[IArray]
      .traverse(vec) { i => () => i }
      .apply()
      .sum

    assert(sumAll == vec.sum)
  }
}

/*
final class IArrayInstancesSuite extends munit.FunSuite {

  test("NonEmptyParallel instance in cats.instances.IArray") {
    import cats.instances.iarray.*
    import cats.syntax.parallel.*

    (IArray(1, 2, 3), IArray("A", "B", "C")).parTupled
  }
}
 */
