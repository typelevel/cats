package cats.tests

import cats.Monad
import cats.data.XorT
import cats.laws.discipline.{MonadTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class XorTTests extends CatsSuite {
  checkAll("Monad[XorT[List, String, Int]]", MonadTests[XorT[List, String, ?]].monad[Int, Int, Int])
  checkAll("Monoid[XorT[List, String, Int]]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("Serializable[Monad[List, String, ?]]]", SerializableTests.serializable(Monad[XorT[List, String, ?]]))
}
