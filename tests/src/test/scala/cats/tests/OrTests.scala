package cats.tests

import algebra.std.int._
import algebra.std.string._
import cats.laws.discipline.FunctorTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import cats.data.Or

class OrTests extends FunSuite with Discipline {
  checkAll("Or[String, Int]", FunctorTests[String Or ?, Int].applicative[Int, Int])
}
