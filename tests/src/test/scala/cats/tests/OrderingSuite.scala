package cats
package tests

import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class OrderingSuite extends CatsSuite {

  Invariant[Ordering]
  Contravariant[Ordering]
  Semigroupal[Ordering]
  ContravariantSemigroupal[Ordering]
  ContravariantMonoidal[Ordering]
  Decideable[Ordering]

  checkAll("Contravariant[Ordering]", ContravariantTests[Ordering].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[Ordering]", SemigroupalTests[Ordering].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Ordering]",
           ContravariantMonoidalTests[Ordering].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("Decideable[Ordering]", DecideableTests[Ordering].decideable[MiniInt, Boolean, Boolean])
  checkAll("Decideable[Ordering]", SerializableTests.serializable(Decideable[Ordering]))
}
