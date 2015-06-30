package cats.tests

import cats.Monad
import cats.data.OptionT
import cats.laws.discipline.{MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class OptionTTests extends CatsSuite {
  checkAll("OptionT[List, Int]", MonadTests[OptionT[List, ?]].monad[Int, Int, Int])
  checkAll("MonadOptionT[List, ?]]", SerializableTests.serializable(Monad[OptionT[List, ?]]))
}
