package cats
package tests

import cats.laws.discipline.{MonoidKTests, SerializableTests}

class SetTests extends CatsSuite {
  checkAll("Set[Int]", MonoidKTests[Set].monoidK[Int])
  checkAll("MonoidK[Set]", SerializableTests.serializable(MonoidK[Set]))
}
