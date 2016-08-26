package cats
package tests

import cats.functor.Functor2
import cats.laws.discipline.{SerializableTests, Functor2Tests}

class Functor2Test extends CatsSuite {
  type Tuple2Either[A, B] = (Either[A, B], Either[A, B])
  val tuple2ComposeEither: Functor2[Tuple2Either] =
    Functor2[Tuple2].compose[Either]

  checkAll("Tuple2 compose Either", Functor2Tests(tuple2ComposeEither).functor2[Int, Int, Int, String, String, String])
  checkAll("Functor2[Tuple2 compose Either]", SerializableTests.serializable(tuple2ComposeEither))
}
