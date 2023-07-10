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

import cats.{Contravariant, ContravariantMonoidal, Invariant}
import cats.kernel.{Order, PartialOrder}
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.{ContravariantMonoidalTests, DeferTests, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.syntax.all._
import cats.tests.Helpers.POrd
import org.scalacheck.Prop._

class PartialOrderSuite extends CatsSuite {

  /**
   * Check that two partial compare results are "the same".
   * This works around the fact that `NaN` is not equal to itself.
   */
  def checkPartialCompare(res1: Double, res2: Double): Unit =
    assert(res1 == res2 || (res1.isNaN && res2.isNaN))

  {
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
  }

  checkAll("PartialOrder", ContravariantMonoidalTests[PartialOrder].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[PartialOrder]", SerializableTests.serializable(ContravariantMonoidal[PartialOrder]))
  checkAll("Defer[PartialOrder]", DeferTests[PartialOrder].defer[MiniInt])

  test("companion object syntax") {
    forAll { (i: Int, j: Int) =>
      val catsKernelStdOrderForInt: Order[Int] = Order[Int]
      checkPartialCompare(PartialOrder.partialCompare(i, j), catsKernelStdOrderForInt.partialCompare(i, j))
      assert(PartialOrder.tryCompare(i, j) === (catsKernelStdOrderForInt.tryCompare(i, j)))
      assert(PartialOrder.pmin(i, j) === (catsKernelStdOrderForInt.pmin(i, j)))
      assert(PartialOrder.pmax(i, j) === (catsKernelStdOrderForInt.pmax(i, j)))
      assert(PartialOrder.lteqv(i, j) === (catsKernelStdOrderForInt.lteqv(i, j)))
      assert(PartialOrder.lt(i, j) === (catsKernelStdOrderForInt.lt(i, j)))
      assert(PartialOrder.gteqv(i, j) === (catsKernelStdOrderForInt.gteqv(i, j)))
      assert(PartialOrder.gt(i, j) === (catsKernelStdOrderForInt.gt(i, j)))
    }
  }

  test("partial order ops syntax") {
    forAll { (i: POrd, j: POrd) =>
      assert((i > j) === (PartialOrder.gt(i, j)))
      assert((i >= j) === (PartialOrder.gteqv(i, j)))
      assert((i < j) === (PartialOrder.lt(i, j)))
      assert((i <= j) === (PartialOrder.lteqv(i, j)))

      checkPartialCompare(i.partialCompare(j), PartialOrder.partialCompare(i, j))
      assert(i.partialComparison(j) === (PartialOrder[POrd].partialComparison(i, j)))
      assert(i.tryCompare(j) === (PartialOrder.tryCompare(i, j)))
      assert(i.pmin(j) === (PartialOrder.pmin(i, j)))
      assert(i.pmax(j) === (PartialOrder.pmax(i, j)))
    }
  }
}

object PartialOrderSuite {
  def summonInstance(): Unit = {
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
    ()
  }
}
