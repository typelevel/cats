package cats
package tests

import cats.data.Xor
import cats.laws.discipline._
import cats.laws.discipline.eq._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class FutureTests extends CatsSuite {
  implicit val eqkf: EqK[Future] =
    new EqK[Future] {
      def synthesize[A: Eq]: Eq[Future[A]] = futureEq(1.second)
    }

  def futureXor[A](f: Future[A]): Future[Xor[Throwable, A]] =
    f.map(Xor.right[Throwable, A]).recover { case t => Xor.left(t) }

  implicit val eqv: Eq[Future[Int]] =
    new Eq[Future[Int]] {
      implicit val throwableEq: Eq[Throwable] = Eq.fromUniversalEquals

      def eqv(x: Future[Int], y: Future[Int]): Boolean =
        futureEq[Xor[Throwable, Int]](1.second).eqv(futureXor(x), futureXor(y))
    }

  implicit val comonad: Comonad[Future] = futureComonad(1.second)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(arbitrary[Exception].map(e => e.asInstanceOf[Throwable]))

  checkAll("Future[Int]", MonadErrorTests[Lambda[(E, A) => Future[A]], Throwable].monadError[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future].comonad[Int, Int, Int])
}
