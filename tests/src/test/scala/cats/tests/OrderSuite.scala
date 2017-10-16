package cats
package tests


import cats.kernel.laws.discipline.{OrderTests => OrderLawTests}

class OrderSuite extends CatsSuite {
  {
    Invariant[Order]
    Contravariant[Order]
  }

  checkAll("Int", OrderLawTests[Int].order)
  checkAll("Double", OrderLawTests[Double].order)
  checkAll("Float", OrderLawTests[Float].order)
  checkAll("Long", OrderLawTests[Long].order)
}

object OrderSuite {
  def summonInstance(): Unit = {
    import cats.instances.order._
    Invariant[Order]
    Contravariant[Order]
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
