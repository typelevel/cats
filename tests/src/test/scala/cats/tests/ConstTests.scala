package cats.tests

import cats.data.Const
import cats.laws.discipline.ApplicativeTests

class ConstTests extends CatsSuite {

  checkAll("Const[String, Int]", ApplicativeTests[Const[String, ?]].applicative[Int, Int, Int])

}
