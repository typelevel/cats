package cats
package tests

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.laws.discipline.{ReducibleTests, SerializableTests}
import cats.laws.discipline.arbitrary.oneAndArbitrary

class ReducibleTest extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  type NEVL[A] = NonEmptyList[NonEmptyVector[A]]
  val nevlReducible: Reducible[NEVL] =
    Reducible[NonEmptyList].compose[NonEmptyVector]
  checkAll("NonEmptyList compose NonEmptyVector", ReducibleTests(nevlReducible).reducible[Option, Int, String])
  checkAll("Reducible[NonEmptyList compose NonEmptyVector]", SerializableTests.serializable(nevlReducible))
}
