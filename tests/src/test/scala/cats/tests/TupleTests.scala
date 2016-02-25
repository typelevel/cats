package cats
package tests

import cats.laws.discipline.{BitraverseTests, SerializableTests}
import cats.laws.discipline.eq.tuple2Eq

class TupleTests extends CatsSuite {
  checkAll("Tuple2", BitraverseTests[Tuple2].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Tuple2]", SerializableTests.serializable(Bitraverse[Tuple2]))
}
