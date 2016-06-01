package cats
package tests

import cats.kernel.laws.GroupLaws

import cats.arrow.Category
import cats.laws.discipline.{MonoidKTests, SerializableTests}
import cats.laws.discipline.eq.function1Eq

class CategoryTest extends CatsSuite {
  val functionCategory = Category[Function1]
  type Endo[A] = Function1[A, A]

  checkAll("Category[Function1].algebraK", MonoidKTests[Endo](functionCategory.algebraK).monoidK[Int])
  checkAll("Category[Function1].algebraK", SerializableTests.serializable(functionCategory.algebraK))

  val functionAlgebra = functionCategory.algebra[Int]
  checkAll("Category[Function1].algebra[Int]", GroupLaws[Endo[Int]].monoid(functionAlgebra))
}
