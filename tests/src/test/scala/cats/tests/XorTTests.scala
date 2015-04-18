package cats.tests

import cats.MonadCombine
import cats.data.XorT
import cats.laws.discipline.{MonadCombineTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class XorTTests extends CatsSuite {
  checkAll("XorT[List, String, Int]", MonadCombineTests[XorT[List, String, ?]].monadCombine[Int, Int, Int])
  checkAll("XorT[List, String, Int]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("MonadCombine[List, String, ?]]", SerializableTests.serializable(MonadCombine[XorT[List, String, ?]]))
}
