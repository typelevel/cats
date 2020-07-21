package cats
package instances

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait FutureInstances extends FutureInstances1 {

  implicit def catsStdInstancesForFuture(implicit
    ec: ExecutionContext
  ): MonadError[Future, Throwable] with CoflatMap[Future] with Monad[Future] =
    new FutureCoflatMap with MonadError[Future, Throwable] with Monad[Future] with StackSafeMonad[Future] {
      override def pure[A](x: A): Future[A] =
        Future.successful(x)
      override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa.flatMap(f)
      override def handleErrorWith[A](fea: Future[A])(f: Throwable => Future[A]): Future[A] =
        fea.recoverWith { case t => f(t) }
      override def raiseError[A](e: Throwable): Future[A] =
        Future.failed(e)
      override def handleError[A](fea: Future[A])(f: Throwable => A): Future[A] =
        fea.recover { case t => f(t) }
      override def attempt[A](fa: Future[A]): Future[Either[Throwable, A]] =
        fa.transformWith(r =>
          Future.successful(
            r match {
              case Success(a) => Right(a)
              case Failure(e) => Left(e)
            }
          )
        )
      override def redeemWith[A, B](fa: Future[A])(recover: Throwable => Future[B], bind: A => Future[B]): Future[B] =
        fa.transformWith {
          case Success(a) => bind(a)
          case Failure(e) => recover(e)
        }
      override def recover[A](fa: Future[A])(pf: PartialFunction[Throwable, A]): Future[A] =
        fa.recover(pf)
      override def recoverWith[A](fa: Future[A])(pf: PartialFunction[Throwable, Future[A]]): Future[A] =
        fa.recoverWith(pf)
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] =
        fa.map(f)
      override def catchNonFatal[A](a: => A)(implicit ev: Throwable <:< Throwable): Future[A] =
        Future(a)
      override def catchNonFatalEval[A](a: Eval[A])(implicit ev: Throwable <:< Throwable): Future[A] =
        Future(a.value)
    }
}

sealed private[instances] trait FutureInstances1 extends FutureInstances2 {
  implicit def catsStdMonoidForFuture[A: Monoid](implicit ec: ExecutionContext): Monoid[Future[A]] =
    new FutureMonoid[A]
}

sealed private[instances] trait FutureInstances2 {
  implicit def catsStdSemigroupForFuture[A: Semigroup](implicit ec: ExecutionContext): Semigroup[Future[A]] =
    new FutureSemigroup[A]
}

abstract private[cats] class FutureCoflatMap(implicit ec: ExecutionContext) extends CoflatMap[Future] {
  def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
}

private[cats] class FutureSemigroup[A: Semigroup](implicit ec: ExecutionContext)
    extends ApplySemigroup[Future, A](future.catsStdInstancesForFuture, implicitly)

private[cats] class FutureMonoid[A](implicit A: Monoid[A], ec: ExecutionContext)
    extends ApplicativeMonoid[Future, A](future.catsStdInstancesForFuture, implicitly)
