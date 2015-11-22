package cats
package js
package tests

import cats.data.Xor
import cats.laws.discipline._
import cats.laws.discipline.eq.tuple3Eq
import cats.js.std.Await
import cats.js.std.future.{futureEq, futureComonad}
import cats.tests.CatsSuite

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class FutureTests extends CatsSuite {
  val timeout = 3.seconds

  def futureXor[A](f: Future[A]): Future[Xor[Throwable, A]] =
    f.map(Xor.right[Throwable, A]).recover { case t => Xor.left(t) }

  implicit def eqfa[A: Eq]: Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(fx: Future[A], fy: Future[A]): Boolean = {
        val fz = futureXor(fx) zip futureXor(fy)
        Await.result(fz.map { case (tx, ty) => tx === ty }, timeout)
      }
    }

  implicit val throwableEq: Eq[Throwable] =
    Eq.fromUniversalEquals

  implicit val comonad: Comonad[Future] = futureComonad(timeout)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(arbitrary[Exception].map(identity))

  checkAll("Future[Int]", MonadErrorTests[Future, Throwable].monadError[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future].comonad[Int, Int, Int])
}
