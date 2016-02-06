package cats
package tests

import cats.functor.Bifunctor
import cats.laws.discipline.{BifunctorTests, SerializableTests}
import cats.laws.discipline.eq.tuple2Eq

class TupleTests extends CatsSuite {
  checkAll("Tuple2", BifunctorTests[Tuple2].bifunctor[Int, Int, Int, String, String, String])
  checkAll("Bifunctor[Tuple2]", SerializableTests.serializable(Bifunctor[Tuple2]))
}
