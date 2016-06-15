package cats
package tests

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.tuple2Eq
import cats.data.{BiProd, Xor}

class BiProdTests extends CatsSuite {
  checkAll("BiProd[Xor, Tuple2, ?, ?]", BitraverseTests[BiProd[Xor, Tuple2, ?, ?]].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[BiProd[Xor, Tuple2, ?, ?]]", SerializableTests.serializable(Bitraverse[BiProd[Xor, Tuple2, ?, ?]]))
}
