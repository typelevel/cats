package cats
package tests


import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class EquivSuite extends CatsSuite {

  Invariant[Equiv]
  Contravariant[Equiv]
  Semigroupal[Equiv]
  ContravariantSemigroupal[Equiv]
  ContravariantMonoidal[Equiv]

  checkAll("Contravariant[Equiv]", ContravariantTests[Equiv].contravariant[Int, Int, Int])
  checkAll("Semigroupal[Equiv]", SemigroupalTests[Equiv].semigroupal[Int, Int, Int])
  checkAll("ContravariantMonoidal[Equiv]",
    ContravariantMonoidalTests[Equiv].contravariantMonoidal[Int, Int, Int])
  checkAll("ContravariantMonoidal[Equiv]",
    SerializableTests.serializable(ContravariantMonoidal[Equiv]))
}
