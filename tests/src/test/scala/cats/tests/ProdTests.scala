package cats
package tests

import cats.data.Prod
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

class ProdTests extends CatsSuite {
  implicit val iso = MonoidalTests.Isomorphisms.covariant[Prod[Option, List, ?]]
  checkAll("Prod[Option, List, Int]", MonoidalTests[Lambda[X => Prod[Option, List, X]]].monoidal[Int, Int, Int])
  checkAll("Monoidal[Prod[Option, List, Int]]", SerializableTests.serializable(Monoidal[Lambda[X => Prod[Option, List, X]]]))

  checkAll("Prod[Option, List, Int]", AlternativeTests[Lambda[X => Prod[Option, List, X]]].alternative[Int, Int, Int])
  checkAll("Alternative[Prod[Option, List, Int]]", SerializableTests.serializable(Alternative[Lambda[X => Prod[Option, List, X]]]))
}
