package cats.tests

import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import cats.Eq
import cats.laws.{ArbitraryK, FunctorLaws}
import cats.std.int._
import cats.std.future._

class FutureTests extends FunSuite with Discipline {
  implicit val future: ArbitraryK[Future] =
    new ArbitraryK[Future] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[Future[A]] =
        Arbitrary(A.arbitrary.map(Future.successful))
    }

  val atMost: FiniteDuration = 1.second

  implicit def eqFuture[A](implicit ev: Eq[A]): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean =
        Await.result((x zip y).map((ev.eqv _).tupled), atMost)
    }

  checkAll("Future[Int]", FunctorLaws[Future, Int].applicative[Int, Int])
}
