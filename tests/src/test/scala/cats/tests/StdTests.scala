package cats.tests

import cats.laws.discipline.{ComonadTests, MonadTests, MonadFilterTests, MonoidKTests}
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
  checkAll("Function0[Int]", ComonadTests[Function0].comonad[Int, Int, Int])
  checkAll("Function0[Int]", MonadTests[Function0].monad[Int, Int, Int])
  checkAll("Option[Int]", MonadFilterTests[Option].monadFilter[Int, Int, Int])
  checkAll("Option[String]", MonadFilterTests[Option].monadFilter[String, Int, Int])
  checkAll("List[Int]", MonadFilterTests[List].monadFilter[Int, Int, Int])
  checkAll("List[Int]", MonoidKTests[List].identity[Int])
  checkAll("Stream[Int]", MonoidKTests[Stream].identity[Int])
  checkAll("Vector[Int]", MonoidKTests[Vector].identity[Int])
}
