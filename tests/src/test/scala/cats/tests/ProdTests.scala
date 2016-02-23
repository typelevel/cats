package cats
package tests

import cats.data.Prod
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

class ProdTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Prod[Option, List, ?]]
  checkAll("Prod[Option, List, Int]", CartesianTests[Lambda[X => Prod[Option, List, X]]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Prod[Option, List, Int]]", SerializableTests.serializable(Cartesian[Lambda[X => Prod[Option, List, X]]]))

  checkAll("Prod[Option, List, Int]", AlternativeTests[Lambda[X => Prod[Option, List, X]]].alternative[Int, Int, Int])
  checkAll("Alternative[Prod[Option, List, Int]]", SerializableTests.serializable(Alternative[Lambda[X => Prod[Option, List, X]]]))
}
