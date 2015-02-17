package cats.tests

import cats.data.Const
import cats.laws.discipline.ApplicativeTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import algebra.std.string._

class ConstTests extends FunSuite with Discipline {

  checkAll("Const[String, Int]", ApplicativeTests[Const[String, ?]].applicative[Int, Int, Int])

}
