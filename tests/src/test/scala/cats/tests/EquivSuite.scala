package cats.tests

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal, Decidable, Invariant, Semigroupal}
import cats.instances.equiv._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class EquivSuite extends CatsSuite {
  Invariant[Equiv]
  Contravariant[Equiv]
  Semigroupal[Equiv]
  ContravariantSemigroupal[Equiv]
  ContravariantMonoidal[Equiv]
  Decidable[Equiv]

  checkAll("Contravariant[Equiv]", ContravariantTests[Equiv].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[Equiv]", SemigroupalTests[Equiv].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Equiv]",
           ContravariantMonoidalTests[Equiv].contravariantMonoidal[MiniInt, Boolean, Boolean]
  )
  checkAll("Decidable[Equiv]", DecidableTests[Equiv].decidable[MiniInt, Boolean, Boolean])
  checkAll("Decidable[Equiv]", SerializableTests.serializable(Decidable[Equiv]))
}
