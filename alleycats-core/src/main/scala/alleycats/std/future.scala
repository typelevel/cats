package alleycats
package std

import scala.concurrent.Future

object future extends FutureInstances

trait FutureInstances {

  implicit val alleycatsStdFuturePure: Pure[Future] =
    new Pure[Future] {
      override def pure[A](a: A): Future[A] = Future.successful(a)
    }
}
