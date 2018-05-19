package cats.internals

import scala.concurrent.{ExecutionContext, Future}

private[cats] object FutureShims {
  /**
   * Exposes the new `Future#transformWith` from Scala 2.12 in a way
   * that's compatible with Scala 2.11.
   */
  def redeemWith[A, B](fa: Future[A])(fe: Throwable => Future[B], fs: A => Future[B])
    (implicit ec: ExecutionContext): Future[B] =
    attempt(fa).flatMap(_.fold(fe, fs))

  /**
   * Exposes an optimized `attempt` for `Future` whose implementation
   * depends on the Scala version.
   */
  def attempt[A](fa: Future[A])(implicit ec: ExecutionContext): Future[Either[Throwable, A]] =
    fa.map(Right[Throwable, A]).recover { case e => Left(e) }
}
