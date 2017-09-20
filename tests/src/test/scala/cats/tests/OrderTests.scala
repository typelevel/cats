package cats
package tests

import cats.functor._
import cats.kernel.laws.discipline.{OrderTests => OrderTypeclassTests}

class OrderTests extends CatsSuite {
  {
    Invariant[Order]
    Contravariant[Order]
  }

  checkAll("Int", OrderTypeclassTests[Int].order)
  checkAll("Double", OrderTypeclassTests[Double].order)
  checkAll("Float", OrderTypeclassTests[Float].order)
  checkAll("Long", OrderTypeclassTests[Long].order)
}

object OrderTests {
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
