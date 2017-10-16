package cats
package tests

import org.scalatest._



class SemigroupSuite extends FunSuite {
  {
    import cats.implicits._
    Invariant[Semigroup]
    Semigroupal[Semigroup]
    InvariantMonoidal[Semigroup]
  }

  {
    import cats.instances.semigroup._
    Invariant[Semigroup]
    Semigroupal[Semigroup]
    InvariantMonoidal[Semigroup]
  }
}
