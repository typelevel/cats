package cats
package tests

import cats.functor._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class PartialOrderingTests extends CatsSuite {

  Invariant[PartialOrdering]
  Contravariant[PartialOrdering]
  Cartesian[PartialOrdering]
  ContravariantCartesian[PartialOrdering]

  checkAll("Contravariant[PartialOrdering]", ContravariantTests[PartialOrdering].contravariant[Int, Int, Int])
  checkAll("Cartesian[PartialOrdering]", CartesianTests[PartialOrdering].cartesian[Int, Int, Int])
  checkAll("Contravariant[PartialOrdering]", SerializableTests.serializable(Contravariant[PartialOrdering]))
}
