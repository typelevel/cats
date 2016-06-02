package cats
package std

import cats.syntax.all._
import cats.data.Xor

import scala.util.control.NonFatal
import scala.concurrent.{ExecutionContext, Future}

trait FutureInstances extends FutureInstances1 {

  implicit def catsStdInstancesForFuture(implicit ec: ExecutionContext): MonadError[Future, Throwable] with CoflatMap[Future] =
    new FutureCoflatMap with MonadError[Future, Throwable]{
      def pure[A](x: A): Future[A] = Future.successful(x)

      override def pureEval[A](x: Eval[A]): Future[A] = x match {
        case Now(x) => Future.successful(x)
        case _ => Future(x.value)
      }

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

      def handleErrorWith[A](fea: Future[A])(f: Throwable => Future[A]): Future[A] = fea.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Future[A] = Future.failed(e)
      override def handleError[A](fea: Future[A])(f: Throwable => A): Future[A] = fea.recover { case t => f(t) }

      override def attempt[A](fa: Future[A]): Future[Throwable Xor A] =
        (fa map Xor.right) recover { case NonFatal(t) => Xor.left(t) }

      override def recover[A](fa: Future[A])(pf: PartialFunction[Throwable, A]): Future[A] = fa.recover(pf)

      override def recoverWith[A](fa: Future[A])(pf: PartialFunction[Throwable, Future[A]]): Future[A] = fa.recoverWith(pf)

      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }
}

private[std] sealed trait FutureInstances1 extends FutureInstances2 {
  implicit def catsStdMonoidForFuture[A: Monoid](implicit ec: ExecutionContext): Monoid[Future[A]] =
    new FutureMonoid[A]
}

private[std] sealed trait FutureInstances2 {
  implicit def catsStdSemigroupForFuture[A: Semigroup](implicit ec: ExecutionContext): Semigroup[Future[A]] =
    new FutureSemigroup[A]
}

private[cats] abstract class FutureCoflatMap(implicit ec: ExecutionContext) extends CoflatMap[Future] {
  def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
}

private[cats] class FutureSemigroup[A: Semigroup](implicit ec: ExecutionContext) extends Semigroup[Future[A]] {
  def combine(fx: Future[A], fy: Future[A]): Future[A] =
    (fx zip fy).map { case (x, y) => x |+| y }
}

private[cats] class FutureMonoid[A](implicit A: Monoid[A], ec: ExecutionContext) extends FutureSemigroup[A] with Monoid[Future[A]] {
  def empty: Future[A] =
    Future.successful(A.empty)
}
