package cats
package tests

import cats.laws.discipline.{FoldableTests, MonoidKTests, SerializableTests}

class SetTests extends CatsSuite {
  checkAll("Set[Int]", MonoidKTests[Set].monoidK[Int])
  checkAll("MonoidK[Set]", SerializableTests.serializable(MonoidK[Set]))

  checkAll("Set[Int]", FoldableTests[Set].foldable[Int, Int])
  checkAll("Foldable[Set]", SerializableTests.serializable(Foldable[Set]))
}
