package cats
package tests

import cats.functor._

import org.scalatest._

class OrderTests extends FunSuite {
  {
    import cats.implicits._
    Invariant[Order]
    Contravariant[Order]
  }

  {
    import cats.instances.order._
    Invariant[Order]
    Contravariant[Order]
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
