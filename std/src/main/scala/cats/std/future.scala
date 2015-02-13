package cats
package std

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

trait FutureInstances {

  implicit def futureInstance(implicit ec: ExecutionContext): Monad[Future] with CoFlatMap[Future] =
    new FutureCoFlatMap with Monad[Future]{

      def pure[A](x: A): Future[A] = Future.successful(x)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }

  def futureComonad(atMost: FiniteDuration)(implicit ec: ExecutionContext): Comonad[Future] =
    new FutureCoFlatMap with Comonad[Future] {

      def extract[A](x: Future[A]): A = Await.result(x, atMost)

      def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }

  implicit def futureSemigroup[A](implicit A: Semigroup[A], ec: ExecutionContext): Semigroup[Future[A]] =
    new FutureSemigroup[A]() {}

  implicit def futureMonoid[A](implicit A: Monoid[A], ec: ExecutionContext): Monoid[Future[A]] =
    new FutureSemigroup[A] with Monoid[Future[A]] {

      def empty: Future[A] = Future.successful(A.empty)
    }

  abstract class FutureCoFlatMap(implicit ec: ExecutionContext) extends CoFlatMap[Future] {

    def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
  }

  class FutureSemigroup[A](implicit A: Semigroup[A], ec: ExecutionContext) extends Semigroup[Future[A]] {

    def combine(fx: Future[A], fy: Future[A]): Future[A] = (fx zip fy).map { case (x, y) => A.combine(x, y) }
  }
}
