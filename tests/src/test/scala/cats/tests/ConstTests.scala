package cats.tests

import cats.data.Const
import cats.laws.discipline.FunctorTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import algebra.std.string._

class ConstTests extends FunSuite with Discipline {

  checkAll("Const[String, Int]", FunctorTests[Const[String, ?], Int].applicative[Int, Int])

}
