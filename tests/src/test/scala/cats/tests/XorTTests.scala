package cats.tests

import cats.Monad
import cats.data.XorT
import cats.laws.discipline.{MonadTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._

import org.scalacheck.Prop.forAll

class XorTTests extends CatsSuite {
  checkAll("XorT[List, String, Int]", MonadTests[XorT[List, String, ?]].monad[Int, Int, Int])
  checkAll("XorT[List, String, Int]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("Monad[XorT[List, String, ?]]", SerializableTests.serializable(Monad[XorT[List, String, ?]]))

  test("toValidated")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toValidated.map(_.toXor) == xort.value
    }
  })

  test("withValidated")(check {
    forAll { (xort: XorT[List, String, Int], f: String => Char, g: Int => Double) =>
      xort.withValidated(_.bimap(f, g)) == xort.bimap(f, g)
    }
  })
}
