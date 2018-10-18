package cats
package tests

import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.ContravariantMonoidalTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]

  checkAll("Eq[Int]", ContravariantMonoidalTests[Eq].contravariantMonoidal[Int, Int, Int])
  checkAll("ContravariantMonoidal[Eq]", SerializableTests.serializable(ContravariantMonoidal[Eq]))

}
