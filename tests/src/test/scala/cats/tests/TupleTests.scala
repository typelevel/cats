package cats
package tests

import cats.laws.discipline.{BifoldableTests, SerializableTests}

class TupleTests extends CatsSuite {
  checkAll("Tuple2", BifoldableTests[Tuple2].bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Tuple2]", SerializableTests.serializable(Bifoldable[Tuple2]))
}
