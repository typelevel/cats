package cats
package tests

import cats.laws.discipline.{ArbitraryK, ApplicativeTests => ApplicativeLawTests}

class ApplicativeTests extends CatsSuite {
  type Prod[A] = (Option[A], List[A])
  implicit val optionListProduct = Applicative[Option].product[List]
  implicit val optionListArbK = ArbitraryK[Option].product[List]
  implicit val optionListEq = Eq.fromUniversalEquals[Prod[Int]]

  checkAll("Applicative#product", ApplicativeLawTests[Prod].applicative[Int, Int, Int])
}
