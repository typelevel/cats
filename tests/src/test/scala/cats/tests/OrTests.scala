package cats.tests

import algebra.std.int._
import algebra.std.string._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import cats.data.Or
import cats.laws.FunctorLaws

class OrTests extends FunSuite with Discipline {
  checkAll("Or[String, Int]", FunctorLaws[String Or ?, Int].applicative[Int, Int])
}
