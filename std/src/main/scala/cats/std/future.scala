package cats
package std

import scala.concurrent.{ExecutionContext, Future}

trait FutureInstances {
  implicit def futureInstance(implicit ec: ExecutionContext): Monad[Future] with CoFlatMap[Future] =
    new Monad[Future] with CoFlatMap[Future] {

      def pure[A](x: A): Future[A] = Future(x)

      def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: Future[A])(f: (Future[A]) => B): Future[B] =
        Future(f(fa))
  }
}
