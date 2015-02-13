package cats.laws.discipline

import algebra.Eq
import org.scalacheck.Arbitrary
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

object eq {

  // create an approximation of Eq[A => B] by generating 100 values for A
  // and compare the application of the two functions
  implicit def function1Eq[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[A => B] = new Eq[A => B] {
    def eqv(f: A => B, g: A => B): Boolean = {
      val samples = List.fill(100)(A.arbitrary.sample).collect{
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two functions")
      }
      samples.forall(s => B.eqv(f(s), g(s)) )
    }
  }

  def futureEq[A](atMost: FiniteDuration)(implicit ev: Eq[A], ec: ExecutionContext): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean =
        Await.result((x zip y).map((ev.eqv _).tupled), atMost)
    }
}
