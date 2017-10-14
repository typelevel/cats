package cats
package tests

import org.scalatest._



class EqTests extends FunSuite {
  {
    import cats.implicits._
    Invariant[Eq]
    Contravariant[Eq]
    Semigroupal[Eq]
    ContravariantSemigroupal[Eq]
  }

  {
    import cats.instances.eq._
    Invariant[Eq]
    Contravariant[Eq]
    Semigroupal[Eq]
    ContravariantSemigroupal[Eq]
  }
}
