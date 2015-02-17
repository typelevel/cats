package cats.tests

import cats.{Comonad, Eq}
import cats.std.int._
import cats.std.future._
import cats.laws.discipline._
import cats.laws.discipline.eq._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureTests extends FunSuite with Discipline {

  implicit val eqv: Eq[Future[Int]] = futureEq(1.second)
  implicit val comonad: Comonad[Future] = futureComonad(1.second)

  checkAll("Future[Int]", ApplicativeTests[Future].applicative[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future].comonad[Int, Int, Int])
}
