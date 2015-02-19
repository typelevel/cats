package cats.tests

import algebra.std.int._
import algebra.std.string._
import cats.laws.discipline.MonadTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import cats.data.Or

class OrTests extends FunSuite with Discipline {
  checkAll("Or[String, Int]", MonadTests[String Or ?].monad[Int, Int, Int])
}
