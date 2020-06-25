package cats.tests

import cats.Endo
import cats.arrow.Category
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{MiniInt, MonoidKTests, SerializableTests}
import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
import cats.laws.discipline.arbitrary.{catsLawsArbitraryForMiniInt, catsLawsCogenForMiniInt}

class CategorySuite extends CatsSuite {
  val functionCategory = Category[Function1]

  checkAll("Category[Function1].algebraK", MonoidKTests[Endo](functionCategory.algebraK).monoidK[MiniInt])
  checkAll("Category[Function1].algebraK", SerializableTests.serializable(functionCategory.algebraK))

  val functionAlgebra = functionCategory.algebra[MiniInt]
  checkAll("Category[Function1].algebra[MiniInt]", MonoidTests[Endo[MiniInt]](functionAlgebra).monoid)
}
