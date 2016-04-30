package cats
package js
package tests

import cats.data.Xor
import cats.laws.discipline._
import cats.js.std.Await
import cats.js.std.future.futureComonad
import cats.tests.CatsSuite

import scala.concurrent.Future
import scala.concurrent.duration._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

// https://issues.scala-lang.org/browse/SI-7934
@deprecated("", "")
class DeprecatedForwarder {
  implicit def runNow = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
}
object DeprecatedForwarder extends DeprecatedForwarder
import DeprecatedForwarder.runNow

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
