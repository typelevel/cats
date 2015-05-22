package cats
package tests

import cats.laws.discipline.{MonoidKTests, SerializableTests}

class SetTests extends CatsSuite {
  checkAll("MonoidK[Set[Int]]", MonoidKTests[Set].monoidK[Int])
  checkAll("Serializable[MonoidK[Set]]", SerializableTests.serializable(MonoidK[Set]))
}
