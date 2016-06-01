package cats
package tests

import cats.functor.Bifunctor
import cats.laws.discipline.{SerializableTests, BifunctorTests}

class BifunctorTest extends CatsSuite {
  type Tuple2Either[A, B] = (Either[A, B], Either[A, B])
  val tuple2ComposeEither: Bifunctor[Tuple2Either] =
    Bifunctor[Tuple2].compose[Either]

  checkAll("Tuple2 compose Either", BifunctorTests(tuple2ComposeEither).bifunctor[Int, Int, Int, String, String, String])
  checkAll("Bifunctor[Tuple2 compose Either]", SerializableTests.serializable(tuple2ComposeEither))
}
