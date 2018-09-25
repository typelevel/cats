package alleycats
package std

import export._

import scala.concurrent.{ExecutionContext, Future}

@reexports(FutureInstances)
object future

object FutureInstances {
  @export(Orphan)
  implicit val exportFuturePure: Pure[Future] =
    new Pure[Future] {
      override def pure[A](a: A): Future[A] = Future.successful(a)
    }

  @export(Orphan)
  implicit def exportFutureOrElse(implicit ec: ExecutionContext): OrElse[Future] =
    new OrElse[Future] {
      override def orElse[A](fa: Future[A], alternative: => Future[A]): Future[A] =
        fa.recoverWith { case _: NoSuchElementException => alternative }
    }
}
