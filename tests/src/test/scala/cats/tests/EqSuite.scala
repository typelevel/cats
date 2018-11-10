package cats
package tests

import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.DecideableTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]
  Decideable[Eq]

  checkAll("Eq[Int]", DecideableTests[Eq].decideable[Int, Int, Int])
  checkAll("Decideable[Eq]", SerializableTests.serializable(Decideable[Eq]))
}
