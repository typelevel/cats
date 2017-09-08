package cats
package tests

import cats.functor._
import cats.kernel.laws.OrderLaws

class OrderTests extends CatsSuite {
  {
    Invariant[Order]
    Contravariant[Order]
  }

  checkAll("Int", OrderLaws[Int].order)
  checkAll("Double", OrderLaws[Double].order)
  checkAll("Float", OrderLaws[Float].order)
  checkAll("Long", OrderLaws[Long].order)
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
