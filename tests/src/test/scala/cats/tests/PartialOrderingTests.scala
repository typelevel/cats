package cats
package tests


import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class PartialOrderingTests extends CatsSuite {

  Invariant[PartialOrdering]
  Contravariant[PartialOrdering]
  Semigroupal[PartialOrdering]
  ContravariantSemigroupal[PartialOrdering]

  checkAll("Contravariant[PartialOrdering]", ContravariantTests[PartialOrdering].contravariant[Int, Int, Int])
  checkAll("Semigroupal[PartialOrdering]", SemigroupalTests[PartialOrdering].semigroupal[Int, Int, Int])
  checkAll("Contravariant[PartialOrdering]", SerializableTests.serializable(Contravariant[PartialOrdering]))
}
