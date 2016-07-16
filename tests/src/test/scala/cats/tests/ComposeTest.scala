package cats
package tests

import cats.kernel.laws.GroupLaws

import cats.arrow.Compose
import cats.laws.discipline.{SemigroupKTests, SerializableTests}
import cats.laws.discipline.eq.catsLawsEqForFn1

class ComposeTest extends CatsSuite {
  val functionCompose = Compose[Function1]
  type Endo[A] = Function1[A, A]

  checkAll("Compose[Function1].algebraK", SemigroupKTests[Endo](functionCompose.algebraK).semigroupK[Int])
  checkAll("Compose[Function1].algebraK", SerializableTests.serializable(functionCompose.algebraK))

  val functionAlgebra = functionCompose.algebra[Int]
  checkAll("Compose[Function1].algebra[Int]", GroupLaws[Endo[Int]].semigroup(functionAlgebra))
}
