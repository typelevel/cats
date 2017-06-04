package cats
package tests

import org.scalatest._

import cats.functor._

class EqTests extends FunSuite {
  {
    import cats.implicits._
    Invariant[Eq]
    Contravariant[Eq]
    Cartesian[Eq]
    ContravariantCartesian[Eq]
  }

  {
    import cats.instances.eq._
    Invariant[Eq]
    Contravariant[Eq]
    Cartesian[Eq]
    ContravariantCartesian[Eq]
  }
}
