package cats.internals

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

private[cats] object FutureShims {
  /**
   * Exposes the new `Future#transformWith` from Scala 2.12 in a way
   * that's compatible with Scala 2.11.
   */
  def redeemWith[A, B](fa: Future[A])(fe: Throwable => Future[B], fs: A => Future[B])
    (implicit ec: ExecutionContext): Future[B] = {

    fa.transformWith {
      case Success(a) => fs(a)
      case Failure(e) => fe(e)
    }
  }

  /**
   * Exposes an optimized `attempt` for `Future` whose implementation
   * depends on the Scala version.
   */
  def attempt[A](fa: Future[A])(implicit ec: ExecutionContext): Future[Either[Throwable, A]] =
    fa.transformWith(r => Future.successful(
      r match {
        case Success(a) => Right(a)
        case Failure(e) => Left(e)
      }))
}
