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
}
