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
import cats.kernel.laws.discipline.{OrderTests, SerializableTests}
import cats.laws.discipline.{ContravariantMonoidalTests, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.tests.Helpers.Ord
import cats.syntax.all._
import org.scalacheck.Prop._

class OrderSuite extends CatsSuite {
  {
    Invariant[Order]
    Contravariant[Order]
    ContravariantMonoidal[Order]
  }

  checkAll("Int", OrderTests[Int].order)
  checkAll("Double", OrderTests[Double].order)
  checkAll("Float", OrderTests[Float].order)
  checkAll("Long", OrderTests[Long].order)

  checkAll("Order", ContravariantMonoidalTests[Order].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Order]", SerializableTests.serializable(ContravariantMonoidal[Order]))

  test("order ops syntax") {
    forAll { (i: Ord, j: Ord) =>
      assert((i.compare(j)) === (Order.compare(i, j)))
      assert((i.min(j)) === (Order.min(i, j)))
      assert((i.max(j)) === (Order.max(i, j)))
      assert((i.comparison(j)) === (Order.comparison(i, j)))

      // partial order syntax should also work when an Order instance exists
      assert((i > j) === (PartialOrder.gt(i, j)))
      assert((i >= j) === (PartialOrder.gteqv(i, j)))
      assert((i < j) === (PartialOrder.lt(i, j)))
      assert((i <= j) === (PartialOrder.lteqv(i, j)))
      assert((i.partialCompare(j)) === (PartialOrder.partialCompare(i, j)))
      assert((i.tryCompare(j)) === (PartialOrder.tryCompare(i, j)))
      assert((i.pmin(j)) === (PartialOrder.pmin(i, j)))
      assert((i.pmax(j)) === (PartialOrder.pmax(i, j)))
    }
  }
}

object OrderSuite {
  def summonInstance(): Unit = {
    Invariant[Order]
    Contravariant[Order]
    ContravariantMonoidal[Order]
    ()
  }

  // ambiguity test:
  // the Ordering instance from the Order instance should be trumped
  // by the one provided in the Ordering companion object
  {
    Ordering[String]
    class C
    implicit val ording: Ordering[C] = new Ordering[C] {
      def compare(x: C, y: C) = 0
    }
    implicit val ord: Order[C] = Order.allEqual
    Ordering[C]
  }
}
