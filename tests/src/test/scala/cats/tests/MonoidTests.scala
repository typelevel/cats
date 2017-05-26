package cats
package tests

import org.scalatest._

import cats.functor._

class MonoidTests extends FunSuite {
  {
    import cats.implicits._
    implicitly[Invariant[Monoid]]
    implicitly[Cartesian[Monoid]]
    implicitly[InvariantMonoidal[Monoid]]
  }

  {
    import cats.instances.monoid._
    implicitly[Invariant[Monoid]]
    implicitly[Cartesian[Monoid]]
    implicitly[InvariantMonoidal[Monoid]]
  }
}
