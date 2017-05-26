package cats
package tests

import org.scalatest._

import cats.functor._

class SemigroupTests extends FunSuite {
  {
    import cats.implicits._
    implicitly[Invariant[Semigroup]]
    implicitly[Cartesian[Semigroup]]
    implicitly[InvariantMonoidal[Semigroup]]
  }

  {
    import cats.instances.semigroup._
    implicitly[Invariant[Semigroup]]
    implicitly[Cartesian[Semigroup]]
    implicitly[InvariantMonoidal[Semigroup]]
  }
}
