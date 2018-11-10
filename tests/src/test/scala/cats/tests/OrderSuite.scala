package cats
package tests

import Helpers.Ord
import cats.kernel.laws.discipline.{OrderTests, SerializableTests}
import cats.laws.discipline.DecideableTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class OrderSuite extends CatsSuite {
  {
    Invariant[Order]
    Contravariant[Order]
    ContravariantMonoidal[Order]
    Decideable[Order]
  }

  checkAll("Int", OrderTests[Int].order)
  checkAll("Double", OrderTests[Double].order)
  checkAll("Float", OrderTests[Float].order)
  checkAll("Long", OrderTests[Long].order)

  checkAll("Order", DecideableTests[Order].decideable[Int, Int, Int])
  checkAll("Decideable[Order]", SerializableTests.serializable(Decideable[Order]))

  test("order ops syntax") {
    forAll { (i: Ord, j: Ord) =>
      (i.compare(j)) should ===(Order.compare(i, j))
      (i.min(j)) should ===(Order.min(i, j))
      (i.max(j)) should ===(Order.max(i, j))
      (i.comparison(j)) should ===(Order.comparison(i, j))

      // partial order syntax should also work when an Order instance exists
      (i > j) should ===(PartialOrder.gt(i, j))
      (i >= j) should ===(PartialOrder.gteqv(i, j))
      (i < j) should ===(PartialOrder.lt(i, j))
      (i <= j) should ===(PartialOrder.lteqv(i, j))
      (i.partialCompare(j)) should ===(PartialOrder.partialCompare(i, j))
      (i.tryCompare(j)) should ===(PartialOrder.tryCompare(i, j))
      (i.pmin(j)) should ===(PartialOrder.pmin(i, j))
      (i.pmax(j)) should ===(PartialOrder.pmax(i, j))
    }
  }
}

object OrderSuite {
  def summonInstance(): Unit = {
    import cats.instances.order._
    Invariant[Order]
    Contravariant[Order]
    ContravariantMonoidal[Order]
    ()
  }

  // ambiguity test:
  // the Ordering instance from the Order instance should be trumped
  // by the one provided in the Ordering companion object
  {
    import cats.instances.all._
    Ordering[String]
    class C
    implicit val ording: Ordering[C] = new Ordering[C] {
      def compare(x: C, y: C) = 0
    }
    implicit val ord: Order[C] = Order.allEqual
    Ordering[C]
  }
}
