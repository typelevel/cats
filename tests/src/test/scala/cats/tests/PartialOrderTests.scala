package cats
package tests

import org.scalatest._

import cats.functor._

class PartialOrderTests extends FunSuite {
  {
    import cats.implicits._
    implicitly[Invariant[PartialOrder]]
    implicitly[Contravariant[PartialOrder]]
  }

  {
    import cats.instances.partialOrder._
    implicitly[Invariant[PartialOrder]]
    implicitly[Contravariant[PartialOrder]]
  }
}
