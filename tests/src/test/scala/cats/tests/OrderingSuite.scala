package cats.tests

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal, Decidable, Invariant, Semigroupal}
import cats.instances.ordering._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class OrderingSuite extends CatsSuite {

  Invariant[Ordering]
  Contravariant[Ordering]
  Semigroupal[Ordering]
  ContravariantSemigroupal[Ordering]
  ContravariantMonoidal[Ordering]
  Decidable[Ordering]

  checkAll("Contravariant[Ordering]", ContravariantTests[Ordering].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[Ordering]", SemigroupalTests[Ordering].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Ordering]",
           ContravariantMonoidalTests[Ordering].contravariantMonoidal[MiniInt, Boolean, Boolean]
  )
  checkAll("Decidable[Ordering]", DecidableTests[Ordering].decidable[MiniInt, Boolean, Boolean])
  checkAll("Decidable[Ordering]", SerializableTests.serializable(Decidable[Ordering]))
}
