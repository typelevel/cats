package cats.tests

import cats.{Comonad, Eq}
import cats.laws.discipline._
import cats.laws.discipline.eq._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureTests extends CatsSuite {

  implicit val eqv: Eq[Future[Int]] = futureEq(1.second)
  implicit val comonad: Comonad[Future] = futureComonad(1.second)

  checkAll("Future[Int]", MonadTests[Future].monad[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future].comonad[Int, Int, Int])
}
