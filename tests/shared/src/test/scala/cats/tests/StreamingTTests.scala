package cats
package tests

import cats.data.StreamingT
import cats.laws.discipline.{EqK, MonadTests, SerializableTests}

class StreamingTTests extends CatsSuite {
  implicit val e: Eq[StreamingT[Eval, Int]] = EqK[StreamingT[Eval, ?]].synthesize[Int]
  checkAll("StreamingT[Eval, ?]", MonadTests[StreamingT[Eval, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[Eval, ?]]", SerializableTests.serializable(Monad[StreamingT[Eval, ?]]))
}
