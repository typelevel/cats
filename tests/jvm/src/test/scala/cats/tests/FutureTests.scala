package cats
package tests

import cats.data.Xor
import cats.laws.discipline._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class FutureTests extends CatsSuite {
  val timeout = 3.seconds

  def futureXor[A](f: Future[A]): Future[Xor[Throwable, A]] =
    f.map(Xor.right[Throwable, A]).recover { case t => Xor.left(t) }

  implicit val eqkf: EqK[Future] =
    new EqK[Future] {
      def synthesize[A: Eq]: Eq[Future[A]] =
        new Eq[Future[A]] {
          def eqv(fx: Future[A], fy: Future[A]): Boolean = {
            val fz = futureXor(fx) zip futureXor(fy)
            Await.result(fz.map { case (tx, ty) => tx === ty }, timeout)
          }
        }
    }

  implicit val throwableEq: Eq[Throwable] =
    Eq.fromUniversalEquals

  implicit val comonad: Comonad[Future] = futureComonad(timeout)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(arbitrary[Exception].map(_.asInstanceOf[Throwable]))

  checkAll("Future[Int]", MonadErrorTests[Lambda[(E, A) => Future[A]], Throwable].monadError[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future].comonad[Int, Int, Int])
}
