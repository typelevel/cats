package alleycats
package std

import export._

import scala.concurrent.Future

@reexports(FutureInstances)
object future

object FutureInstances {
  @export(Orphan)
  implicit val exportFuturePure: Pure[Future] =
    new Pure[Future] {
      override def pure[A](a: A): Future[A] = Future.successful(a)
    }
}
