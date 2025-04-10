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
import cats.laws.discipline.{ContravariantMonoidalTests, DeferTests, MiniInt}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.tests.Helpers.Ord
import cats.syntax.all.*
import org.scalacheck.Prop.*

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
  checkAll("Defer[Order]", DeferTests[Order].defer[MiniInt])

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

  test("Order.fromComparable") {
    val OrderOfCmp = Order.fromComparable[OrderSuite.Cmp]
    assert(OrderOfCmp.lt(OrderSuite.Cmp(1), OrderSuite.Cmp(2)))
    assert(OrderOfCmp.gt(OrderSuite.Cmp(2), OrderSuite.Cmp(1)))
    assert(OrderOfCmp.eqv(OrderSuite.Cmp(1), OrderSuite.Cmp(1)))
    val OrderOfCmpSub = Order.fromComparable[OrderSuite.CmpSub]
    assert(OrderOfCmpSub.lt(OrderSuite.CmpSub(1, "ignored"), OrderSuite.CmpSub(2, "ignored")))
    assert(OrderOfCmpSub.gt(OrderSuite.CmpSub(2, "ignored"), OrderSuite.CmpSub(1, "ignored")))
    assert(OrderOfCmpSub.eqv(OrderSuite.CmpSub(1, "a"), OrderSuite.CmpSub(1, "b")))
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

  class Cmp(protected val n: Int) extends Comparable[Cmp] {
    override def compareTo(o: Cmp): Int = n.compare(o.n)
  }
  object Cmp {
    def apply(n: Int): Cmp = new Cmp(n)
  }
  class CmpSub(override protected val n: Int, private val ignored: String) extends Cmp(n)
  object CmpSub {
    def apply(n: Int, ignored: String): CmpSub = new CmpSub(n, ignored)
  }
}
