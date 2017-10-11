package cats
package instances

import scala.util.control.NonFatal
import scala.concurrent.{ExecutionContext, Future}

trait FutureInstances extends FutureInstances1 {

  implicit def catsStdInstancesForFuture(implicit ec: ExecutionContext): MonadError[Future, Throwable] with CoflatMap[Future] with Monad[Future] =
    new FutureCoflatMap with MonadError[Future, Throwable] with Monad[Future] with StackSafeMonad[Future] {
      def pure[A](x: A): Future[A] = Future.successful(x)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

      def handleErrorWith[A](fea: Future[A])(f: Throwable => Future[A]): Future[A] = fea.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Future[A] = Future.failed(e)
      override def handleError[A](fea: Future[A])(f: Throwable => A): Future[A] = fea.recover { case t => f(t) }

      override def attempt[A](fa: Future[A]): Future[Either[Throwable, A]] =
        (fa.map(a => Right[Throwable, A](a))) recover { case NonFatal(t) => Left(t) }

      override def recover[A](fa: Future[A])(pf: PartialFunction[Throwable, A]): Future[A] = fa.recover(pf)

      override def recoverWith[A](fa: Future[A])(pf: PartialFunction[Throwable, Future[A]]): Future[A] = fa.recoverWith(pf)

      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

      override def catchNonFatal[A](a: => A)(implicit ev: Throwable <:< Throwable): Future[A] = Future(a)

      override def catchNonFatalEval[A](a: Eval[A])(implicit ev: Throwable <:< Throwable): Future[A] = Future(a.value)
    }
}

private[instances] sealed trait FutureInstances1 extends FutureInstances2 {
  implicit def catsStdMonoidForFuture[A: Monoid](implicit ec: ExecutionContext): Monoid[Future[A]] =
    new FutureMonoid[A]
}

private[instances] sealed trait FutureInstances2 {
  implicit def catsStdSemigroupForFuture[A: Semigroup](implicit ec: ExecutionContext): Semigroup[Future[A]] =
    new FutureSemigroup[A]
}

private[cats] abstract class FutureCoflatMap(implicit ec: ExecutionContext) extends CoflatMap[Future] {
  def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
}

private[cats] class FutureSemigroup[A: Semigroup](implicit ec: ExecutionContext)
  extends ApplySemigroup[Future, A](future.catsStdInstancesForFuture, implicitly)

private[cats] class FutureMonoid[A](implicit A: Monoid[A], ec: ExecutionContext)
  extends ApplicativeMonoid[Future, A](future.catsStdInstancesForFuture, implicitly)
