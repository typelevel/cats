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

import cats.{Align, Alternative, CoflatMap, Eval, Monad, MonoidK, Traverse, TraverseFilter}
import cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder}
import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests}
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  MonadTests,
  SerializableTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.eq._
import scala.collection.immutable.ArraySeq
import org.scalacheck.Prop._

class ArraySeqSuite extends CatsSuite {
  checkAll("ArraySeq[Int]", MonoidTests[ArraySeq[Int]].monoid)
  checkAll("Monoid[ArraySeq]", SerializableTests.serializable(Monoid[ArraySeq[Int]]))

  checkAll("ArraySeq[Int]", OrderTests[ArraySeq[Int]].order)
  checkAll("Order[ArraySeq]", SerializableTests.serializable(Order[ArraySeq[Int]]))

  checkAll("ArraySeq[Int]", CoflatMapTests[ArraySeq].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[ArraySeq]", SerializableTests.serializable(CoflatMap[ArraySeq]))

  checkAll("ArraySeq[Int]", AlternativeTests[ArraySeq].alternative[Int, Int, Int])
  checkAll("Alternative[ArraySeq]", SerializableTests.serializable(Alternative[ArraySeq]))

  // Traverse behaviour discriminates on the Runtime type of the Applicative
  checkAll("ArraySeq[Int] with Option", TraverseTests[ArraySeq].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("ArraySeq[Int] with Eval", TraverseTests[ArraySeq].traverse[Int, Int, Int, Set[Int], Eval, Eval])
  checkAll("Traverse[ArraySeq]", SerializableTests.serializable(Traverse[ArraySeq]))

  checkAll("ArraySeq[Int]", MonadTests[ArraySeq].monad[Int, Int, Int])
  checkAll("Monad[ArraySeq]", SerializableTests.serializable(Monad[ArraySeq]))

  checkAll("ArraySeq[Int]", TraverseFilterTests[ArraySeq].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[ArraySeq]", SerializableTests.serializable(TraverseFilter[ArraySeq]))

  checkAll("ArraySeq[Int]", AlignTests[ArraySeq].align[Int, Int, Int, Int])
  checkAll("Align[ArraySeq]", SerializableTests.serializable(Align[ArraySeq]))

  {
    implicit val eqv: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("ArraySeq[Int]", EqTests[ArraySeq[ListWrapper[Int]]].eqv)
    checkAll("Eq[ArraySeq]", SerializableTests.serializable(Eq[ArraySeq[ListWrapper[Int]]]))
  }

  {
    implicit val partialOrder: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("ArraySeq[Int]", PartialOrderTests[ArraySeq[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[ArraySeq]", SerializableTests.serializable(PartialOrder[ArraySeq[ListWrapper[Int]]]))
  }

  {
    implicit val hash: Hash[ListWrapper[Int]] = ListWrapper.hash[Int]
    checkAll("ArraySeq[Int]", HashTests[ArraySeq[ListWrapper[Int]]].hash)
    checkAll("Hash[ArraySeq]", SerializableTests.serializable(Hash[ArraySeq[ListWrapper[Int]]]))
  }

  test("show") {
    assert(ArraySeq(1, 2, 3).show === s"ArraySeq(1, 2, 3)")
    assert(ArraySeq.empty[Int].show === s"ArraySeq()")
  }

  test("MonoidK.algebra consistent with Monoid") {
    forAll { (xs: ArraySeq[Int], ys: ArraySeq[Int]) =>
      assert(MonoidK[ArraySeq].algebra[Int].combine(xs, ys) === (Monoid[ArraySeq[Int]].combine(xs, ys)))
    }
  }
}
