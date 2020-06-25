package cats.tests

import cats.Endo
import cats.arrow.Compose
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline.{MiniInt, SemigroupKTests, SerializableTests}
import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
import cats.laws.discipline.arbitrary._
import cats.syntax.compose._

class ComposeSuite extends CatsSuite {
  val functionCompose = Compose[Function1]

  checkAll("Compose[Function1].algebraK", SemigroupKTests[Endo](functionCompose.algebraK).semigroupK[MiniInt])
  checkAll("Compose[Function1].algebraK", SerializableTests.serializable(functionCompose.algebraK))

  val functionAlgebra = functionCompose.algebra[MiniInt]
  checkAll("Compose[Function1].algebra[MiniInt]", SemigroupTests[Endo[MiniInt]](functionAlgebra).semigroup)

  test("syntax") {
    (((_: Int) + 1) <<< ((_: Int) / 2))(2) should be(2)
    (((_: Int) + 1) >>> ((_: Int) / 2))(5) should be(3)
  }
}
