package cats
package laws

import algebra.laws._
import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite

// TODO: eventually all the algebra.std instances should be available
// via cats.std and we can remove the algebra imports
import algebra.std.int._
import algebra.std.string._
import cats.std.function._
import cats.std.list._
import cats.std.option._

class LawTests extends FunSuite with Discipline {
  checkAll("Function0[Int]", FunctorLaws[Function0, Int].applicative[Int, Int])
  checkAll("Function0[Int]", ComonadLaws[Function0, Int, Int].comonad[Int])
  checkAll("Option[Int]", FunctorLaws[Option, Int].applicative[Int, Int])
  checkAll("Option[String]", FunctorLaws[Option, String].applicative[Int, Int])
  checkAll("List[Int]", FunctorLaws[List, Int].applicative[Int, Int])
}
