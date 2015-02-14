package cats.tests

import cats.laws.discipline.{MonoidKTests, ComonadTests, FunctorTests}
import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

// TODO: eventually all the algebra.std instances should be available
// via cats.std and we can remove the algebra imports
import algebra.std.int._
import algebra.std.string._
import cats.std.function._
import cats.std.list._
import cats.std.option._
import cats.std.stream._
import cats.std.vector._

class StdTests extends FunSuite with Discipline {
  checkAll("Function0[Int]", FunctorTests[Function0, Int].applicative[Int, Int])
  checkAll("Function0[Int]", ComonadTests[Function0, Int, Int].comonad[Int])
  checkAll("Option[Int]", FunctorTests[Option, Int].applicative[Int, Int])
  checkAll("Option[String]", FunctorTests[Option, String].applicative[Int, Int])
  checkAll("Option[String]", MonoidKTests[Option, String].identity)
  checkAll("List[Int]", FunctorTests[List, Int].applicative[Int, Int])
  checkAll("List[Int]", MonoidKTests[List, Int].identity)
  checkAll("Stream[Int]", MonoidKTests[Stream, Int].identity)
  checkAll("Vector[Int]", MonoidKTests[Vector, Int].identity)
}
