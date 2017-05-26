package cats
package tests

import org.scalatest._

import cats.functor._

class EqTests extends FunSuite {
  {
    import cats.implicits._
    implicitly[Invariant[Eq]]
    implicitly[Contravariant[Eq]]
    implicitly[Cartesian[Eq]]
    implicitly[ContravariantCartesian[Eq]]
  }

  {
    import cats.instances.eq._
    implicitly[Invariant[Eq]]
    implicitly[Contravariant[Eq]]
    implicitly[Cartesian[Eq]]
    implicitly[ContravariantCartesian[Eq]]
  }
}
