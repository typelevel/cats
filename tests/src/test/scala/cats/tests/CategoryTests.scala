package cats
package tests

import cats.kernel.laws.discipline.{MonoidTests => MonoidLawTests}

import cats.arrow.Category
import cats.laws.discipline.{MonoidKTests, SerializableTests}
import cats.laws.discipline.eq.catsLawsEqForFn1

class CategoryTest extends CatsSuite {
  val functionCategory = Category[Function1]
  type Endo[A] = Function1[A, A]

  checkAll("Category[Function1].algebraK", MonoidKTests[Endo](functionCategory.algebraK).monoidK[Int])
  checkAll("Category[Function1].algebraK", SerializableTests.serializable(functionCategory.algebraK))

  val functionAlgebra = functionCategory.algebra[Int]
  checkAll("Category[Function1].algebra[Int]", MonoidLawTests[Endo[Int]](functionAlgebra).monoid)
}
