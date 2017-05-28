package cats
package tests

import org.scalatest._

import cats.functor._

class MonoidTests extends FunSuite {
  {
    import cats.implicits._
    Invariant[Monoid]
    Cartesian[Monoid]
    InvariantMonoidal[Monoid]
  }

  {
    import cats.instances.monoid._
    Invariant[Monoid]
    Cartesian[Monoid]
    InvariantMonoidal[Monoid]
  }
}
