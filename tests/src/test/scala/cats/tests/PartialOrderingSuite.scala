package cats.tests

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal, Invariant, Semigroupal}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class PartialOrderingSuite extends CatsSuite {

  Invariant[PartialOrdering]
  Contravariant[PartialOrdering]
  Semigroupal[PartialOrdering]
  ContravariantSemigroupal[PartialOrdering]

  checkAll("Contravariant[PartialOrdering]", ContravariantTests[PartialOrdering].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[PartialOrdering]", SemigroupalTests[PartialOrdering].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("Contravariant[PartialOrdering]", SerializableTests.serializable(Contravariant[PartialOrdering]))

  checkAll("PartialOrdering[Int]",
           ContravariantMonoidalTests[PartialOrdering].contravariantMonoidal[MiniInt, Boolean, Boolean]
  )
  checkAll("ContravariantMonoidal[PartialOrdering]",
           SerializableTests.serializable(ContravariantMonoidal[PartialOrdering])
  )
}
