package cats
package tests

import cats.data.StreamingT
import cats.laws.discipline.{EqK, MonadTests, SerializableTests}

class StreamingTTests extends CatsSuite {

  implicit val e0: Eq[StreamingT[Eval, Int]] = EqK[StreamingT[Eval, ?]].synthesize[Int]
  checkAll("StreamingT[Eval, ?]", MonadTests[StreamingT[Eval, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[Eval, ?]]", SerializableTests.serializable(Monad[StreamingT[Eval, ?]]))

  implicit val e1: Eq[StreamingT[Option, Int]] = EqK[StreamingT[Option, ?]].synthesize[Int]
  checkAll("StreamingT[Option, ?]", MonadTests[StreamingT[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[Option, ?]]", SerializableTests.serializable(Monad[StreamingT[Option, ?]]))

  implicit val e2: Eq[StreamingT[List, Int]] = EqK[StreamingT[List, ?]].synthesize[Int]
  checkAll("StreamingT[List, ?]", MonadTests[StreamingT[List, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[List, ?]]", SerializableTests.serializable(Monad[StreamingT[List, ?]]))
}
