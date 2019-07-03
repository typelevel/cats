package cats
package tests


import cats.data.NonEmptyVector

import cats.laws.discipline.arbitrary._


class LegacyNonEmptyVectorSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)


  test("NonEmptyVector#toString produces correct output") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.toString should ===(s"NonEmpty${nonEmptyVector.toVector.toString}")
    }
    NonEmptyVector(1, Vector.empty).toString should ===("NonEmptyVector(1)")
    NonEmptyVector(1, Vector.empty).toVector.toString should ===("Vector(1)")
  }

}
