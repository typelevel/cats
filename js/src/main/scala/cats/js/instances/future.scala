package cats
package js
package instances

import scala.concurrent.Future
import scala.concurrent.{ExecutionContext => E}
import scala.concurrent.duration.FiniteDuration

import cats.instances.FutureCoflatMap
import cats.syntax.all._

object future extends FutureInstances0

object Await {
  def result[A](f: Future[A], atMost: FiniteDuration): A = f.value match {
    case Some(v) => v.get
    case None => throw new IllegalStateException()
  }
}

private[instances] sealed trait FutureInstances0 extends FutureInstances1 {
  def futureComonad(atMost: FiniteDuration)(implicit ec: E): Comonad[Future] =
    new FutureCoflatMap with Comonad[Future] {
      def extract[A](x: Future[A]): A =
        Await.result(x, atMost)
    }

  def futureOrder[A: Order](atMost: FiniteDuration)(implicit ec: E): Order[Future[A]] =
    new Order[Future[A]] {
      def compare(x: Future[A], y: Future[A]): Int =
        Await.result((x zip y).map { case (x, y) => x compare y }, atMost)
    }
}

private[instances] sealed trait FutureInstances1 extends FutureInstances2 {
  def futurePartialOrder[A: PartialOrder](atMost: FiniteDuration)(implicit ec: E): PartialOrder[Future[A]] =
    new PartialOrder[Future[A]] {
      def partialCompare(x: Future[A], y: Future[A]): Double =
        Await.result((x zip y).map { case (x, y) => x partialCompare y }, atMost)
    }

}

private[instances] sealed trait FutureInstances2 {
  def futureEq[A: Eq](atMost: FiniteDuration)(implicit ec: E): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean =
        Await.result((x zip y).map { case (x, y) => x === y }, atMost)
    }
}
