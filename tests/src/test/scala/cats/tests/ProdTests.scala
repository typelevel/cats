package cats
package tests

import cats.data.Prod
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class ProdTests extends CatsSuite {
  checkAll("Prod[Option, List, Int]", AlternativeTests[Lambda[X => Prod[Option, List, X]]].alternative[Int, Int, Int])
  checkAll("Alternative[Prod[Option, List, Int]]", SerializableTests.serializable(Alternative[Lambda[X => Prod[Option, List, X]]]))
}
