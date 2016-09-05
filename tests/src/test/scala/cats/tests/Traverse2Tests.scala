package cats
package tests

import cats.laws.discipline.{Traverse2Tests, SerializableTests}

class Traverse2Test extends CatsSuite {
  type EitherTuple2[A, B] = Either[(A, B), (A, B)]
  val eitherComposeTuple2: Traverse2[EitherTuple2] =
    Traverse2[Either].compose[Tuple2]

  checkAll("Either compose Tuple2", Traverse2Tests(eitherComposeTuple2).traverse2[Option, Int, Int, Int, String, String, String])
  checkAll("Traverse2[Either compose Tuple2]", SerializableTests.serializable(eitherComposeTuple2))
}
