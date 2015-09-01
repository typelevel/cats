package cats
package jvm
package std

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

import cats.std.FutureCoflatMap
import cats.syntax.all._

object future {

  type E = ExecutionContext

  def futureEq[A: Eq](atMost: FiniteDuration)(implicit ec: E): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean =
        Await.result((x zip y).map { case (x, y) => x === y }, atMost)
    }

  def futurePartialOrder[A: PartialOrder](atMost: FiniteDuration)(implicit ec: E): PartialOrder[Future[A]] =
    new PartialOrder[Future[A]] {
      def partialCompare(x: Future[A], y: Future[A]): Double =
        Await.result((x zip y).map { case (x, y) => x partialCompare y }, atMost)
    }

  def futureOrder[A: Order](atMost: FiniteDuration)(implicit ec: E): Eq[Future[A]] =
    new Order[Future[A]] {
      def compare(x: Future[A], y: Future[A]): Int =
        Await.result((x zip y).map { case (x, y) => x compare y }, atMost)
    }

  def futureComonad(atMost: FiniteDuration)(implicit ec: E): Comonad[Future] =
    new FutureCoflatMap with Comonad[Future] {
      def extract[A](x: Future[A]): A =
        Await.result(x, atMost)
    }
}
