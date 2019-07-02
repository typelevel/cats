package cats
package tests

import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._


class EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]
  Decideable[Eq]

  checkAll("Eq", DecideableTests[Eq].decideable[MiniInt, Boolean, Boolean])
  checkAll("Decideable[Eq]", SerializableTests.serializable(Decideable[Eq]))
}
