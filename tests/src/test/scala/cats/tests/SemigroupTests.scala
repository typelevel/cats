package cats
package tests

import org.scalatest._



class SemigroupTests extends FunSuite {
  {
    import cats.implicits._
    Invariant[Semigroup]
    Cartesian[Semigroup]
    InvariantMonoidal[Semigroup]
  }

  {
    import cats.instances.invariant._
    Invariant[Semigroup]
    Cartesian[Semigroup]
    InvariantMonoidal[Semigroup]
  }
}
