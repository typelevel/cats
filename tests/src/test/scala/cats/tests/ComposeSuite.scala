package cats
package tests

import cats.kernel.laws.discipline.SemigroupTests
import cats.arrow.Compose
import cats.laws.discipline.{SemigroupKTests, SerializableTests}
import cats.laws.discipline.eq.catsLawsEqForFn1

class ComposeSuite extends CatsSuite {
  val functionCompose = Compose[Function1]

  checkAll("Compose[Function1].algebraK", SemigroupKTests[Endo](functionCompose.algebraK).semigroupK[Int])
  checkAll("Compose[Function1].algebraK", SerializableTests.serializable(functionCompose.algebraK))

  val functionAlgebra = functionCompose.algebra[Int]
  checkAll("Compose[Function1].algebra[Int]", SemigroupTests[Endo[Int]](functionAlgebra).semigroup)

  test("syntax") {
    (((_: Int) + 1) <<< ((_: Int) / 2))(2) should be(2)
    (((_: Int) + 1) >>> ((_: Int) / 2))(5) should be(3)
  }
}
