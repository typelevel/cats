package cats
package tests

import cats.laws.discipline.{Foldable2Tests, SerializableTests}

class Foldable2Test extends CatsSuite {
  type EitherEither[A, B] = Either[Either[A, B], Either[A, B]]
  val eitherComposeEither: Foldable2[EitherEither] =
    Foldable2[Either].compose[Either]

  checkAll("Either compose Either", Foldable2Tests(eitherComposeEither).foldable2[Int, Int, Int])
  checkAll("Foldable2[Either compose Either]", SerializableTests.serializable(eitherComposeEither))
}
