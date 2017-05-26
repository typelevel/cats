package cats
package tests

import cats.functor._

import org.scalatest._

class OrderTests extends FunSuite {
  {
    import cats.implicits._
    implicitly[Invariant[Order]]
    implicitly[Contravariant[Order]]
  }

  {
    import cats.instances.order._
    implicitly[Invariant[Order]]
    implicitly[Contravariant[Order]]
  }
}
