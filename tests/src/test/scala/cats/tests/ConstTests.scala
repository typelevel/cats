package cats.tests

import cats.data.Const
import cats.laws.FunctorLaws
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import algebra.std.string._

class ConstTests extends FunSuite with Discipline {

  checkAll("Const[String, Int]", FunctorLaws[Const[String, ?], Int].applicative[Int, Int])

}
