package cats
package tests

import org.scalatest._

import cats.functor._

class PartialOrderTests extends FunSuite {
  {
    import cats.implicits._
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
  }

  {
    import cats.instances.partialOrder._
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
  }
}
