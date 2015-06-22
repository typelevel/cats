package cats
package std

import cats.syntax.eq._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

trait FutureInstances extends FutureInstances1 {

  implicit def futureInstance(implicit ec: ExecutionContext): MonadError[Lambda[(E, A) => Future[A]], Throwable] with CoflatMap[Future] =
    new FutureCoflatMap with MonadError[Lambda[(E, A) => Future[A]], Throwable]{
      def pure[A](x: A): Future[A] = Future.successful(x)

      override def pureEval[A](x: Eval[A]): Future[A] = Future(x.value)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

      def handleError[A](fea: Future[A])(f: Throwable => Future[A]): Future[A] = fea.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Future[A] = Future.failed(e)

      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }

  implicit def futureSemigroup[A](implicit A: Semigroup[A], ec: ExecutionContext): Semigroup[Future[A]] =
    new FutureSemigroup[A]()

  def futureEq[A](atMost: FiniteDuration)(implicit A: Eq[A], ec: ExecutionContext): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(fx: Future[A], fy: Future[A]): Boolean =
        Await.result((fx zip fy).map { case (x, y) => x === y }, atMost)
    }
}

trait FutureInstances1 {

  def futureComonad(atMost: FiniteDuration)(implicit ec: ExecutionContext): Comonad[Future] =
    new FutureCoflatMap with Comonad[Future] {

      def extract[A](x: Future[A]): A = Await.result(x, atMost)

      def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }

  implicit def futureMonoid[A](implicit A: Monoid[A], ec: ExecutionContext): Monoid[Future[A]] =
    new FutureSemigroup[A] with Monoid[Future[A]] {

      def empty: Future[A] = Future.successful(A.empty)
    }
}

private[std] abstract class FutureCoflatMap(implicit ec: ExecutionContext) extends CoflatMap[Future] {

  def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
}

private[std] class FutureSemigroup[A](implicit A: Semigroup[A], ec: ExecutionContext) extends Semigroup[Future[A]] {

  def combine(fx: Future[A], fy: Future[A]): Future[A] = (fx zip fy).map((A.combine _).tupled)
}
