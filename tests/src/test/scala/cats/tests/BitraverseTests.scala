package cats
package tests

import cats.data.Xor
import cats.laws.discipline.{BitraverseTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class BitraverseTest extends CatsSuite {
  type XorTuple2[A, B] = Xor[(A, B), (A, B)]
  val xorComposeTuple2: Bitraverse[XorTuple2] =
    Bitraverse[Xor].compose[Tuple2]

  checkAll("Xor compose Tuple2", BitraverseTests(xorComposeTuple2).bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Xor compose Tuple2]", SerializableTests.serializable(xorComposeTuple2))
}
