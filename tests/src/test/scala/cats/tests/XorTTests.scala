package cats.tests

import cats.MonadError
import cats.data.XorT
import cats.laws.discipline.{MonadErrorTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class XorTTests extends CatsSuite {
  checkAll("XorT[List, String, Int]", MonadErrorTests[XorT[List, ?, ?], String].monadError[Int, Int, Int])
  checkAll("XorT[List, String, Int]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("MonadError[XorT[List, ?, ?]]", SerializableTests.serializable(MonadError[XorT[List, ?, ?], String]))
}
