package cats.tests

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal, Invariant, Semigroupal}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class EquivSuite extends CatsSuite {

  Invariant[Equiv]
  Contravariant[Equiv]
  Semigroupal[Equiv]
  ContravariantSemigroupal[Equiv]
  ContravariantMonoidal[Equiv]

  checkAll("Contravariant[Equiv]", ContravariantTests[Equiv].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[Equiv]", SemigroupalTests[Equiv].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Equiv]",
           ContravariantMonoidalTests[Equiv].contravariantMonoidal[MiniInt, Boolean, Boolean]
  )
  checkAll("ContravariantMonoidal[Equiv]", SerializableTests.serializable(ContravariantMonoidal[Equiv]))
}
