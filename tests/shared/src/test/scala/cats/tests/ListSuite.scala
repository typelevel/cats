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
import cats.data.{NonEmptyList, ZipList}
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
import cats.syntax.list._
import cats.syntax.show._
import cats.syntax.eq._
import org.scalacheck.Prop._

class ListSuite extends CatsSuite {

  checkAll("List[Int]", SemigroupalTests[List].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[List]", SerializableTests.serializable(Semigroupal[List]))

  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", AlternativeTests[List].alternative[Int, Int, Int])
  checkAll("Alternative[List]", SerializableTests.serializable(Alternative[List]))

  // Traverse behaviour discriminates on the Runtime type of the Applicative
  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("List[Int] with Eval", TraverseTests[List].traverse[Int, Int, Int, Set[Int], Eval, Eval])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  checkAll("List[Int]", MonadTests[List].monad[Int, Int, Int])
  checkAll("Monad[List]", SerializableTests.serializable(Monad[List]))

  checkAll("List[Int]", TraverseFilterTests[List].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[List]", SerializableTests.serializable(TraverseFilter[List]))

  checkAll("List[Int]", AlignTests[List].align[Int, Int, Int, Int])
  checkAll("Align[List]", SerializableTests.serializable(Align[List]))

  checkAll("List[Int]", ShortCircuitingTests[List].traverseFilter[Int])
  checkAll("List[Int]", ShortCircuitingTests[List].foldable[Int])

  checkAll("ZipList[Int]", CommutativeApplyTests[ZipList].commutativeApply[Int, Int, Int])

  test("nel => list => nel returns original nel")(
    forAll { (fa: NonEmptyList[Int]) =>
      assert(fa.toList.toNel === (Some(fa)))
    }
  )

  test("toNel on empty list returns None") {
    assert(List.empty[Int].toNel === None)
  }

  test("groupByNel should be consistent with groupBy")(
    forAll { (fa: List[Int], f: Int => Int) =>
      assert((fa.groupByNel(f).map { case (k, v) => (k, v.toList) }: Map[Int, List[Int]]) === fa.groupBy(f))
    }
  )

  test("groupByNelA should be consistent with groupByNel")(
    forAll { (fa: List[Int], f: Int => Int) =>
      assert(fa.groupByNelA(f.andThen(Option(_))) === (Option(fa.groupByNel(f))))
    }
  )

  test("scanLeftNel should be consistent with scanLeft")(
    forAll { (fa: List[Int], b: Int, f: (Int, Int) => Int) =>
      assert(fa.scanLeftNel(b)(f).toList === fa.scanLeft(b)(f))
    }
  )

  test("scanRightNel should be consistent with scanRight")(
    forAll { (fa: List[Int], b: Int, f: (Int, Int) => Int) =>
      assert(fa.scanRightNel(b)(f).toList === fa.scanRight(b)(f))
    }
  )

  test("show") {
    assert(List(1, 2, 3).show === "List(1, 2, 3)")
    assert((Nil: List[Int]).show === "List()")
    forAll { (l: List[String]) =>
      assert(l.show === (l.toString))
    }
  }

  test("traverse is stack-safe") {
    val lst = (0 until 100000).toList
    val sumAll = Traverse[List]
      .traverse(lst) { i => () => i }
      .apply()
      .sum

    assert(sumAll == lst.sum)
  }
}

final class ListInstancesSuite extends munit.FunSuite {

  test("NonEmptyParallel instance in cats.instances.list") {
    import cats.instances.list._
    import cats.syntax.parallel._

    (List(1, 2, 3), List("A", "B", "C")).parTupled
  }
}
