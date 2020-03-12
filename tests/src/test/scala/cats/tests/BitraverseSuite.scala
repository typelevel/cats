package cats.tests

import cats.Bitraverse
import cats.instances.all._
import cats.laws.discipline.{BitraverseTests, SerializableTests}

class BitraverseSuite extends CatsSuite {
  type EitherTuple2[A, B] = Either[(A, B), (A, B)]
  val eitherComposeTuple2: Bitraverse[EitherTuple2] =
    Bitraverse[Either].compose[Tuple2]

  checkAll("Either compose Tuple2",
           BitraverseTests(eitherComposeTuple2).bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Either compose Tuple2]", SerializableTests.serializable(eitherComposeTuple2))
}
