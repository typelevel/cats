package cats
package tests

import org.scalatest._



class EqSuite extends FunSuite {
  {
    import cats.implicits._
    Invariant[Eq]
    Contravariant[Eq]
    Semigroupal[Eq]
    ContravariantSemigroupal[Eq]
    ContravariantMonoidal[Eq]
  }

  {
    import cats.instances.eq._
    Invariant[Eq]
    Contravariant[Eq]
    Semigroupal[Eq]
    ContravariantSemigroupal[Eq]
    ContravariantMonoidal[Eq]
  }
}
