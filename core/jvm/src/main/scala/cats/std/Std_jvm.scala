package cats
package std

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration

private[std] object Platform {

  trait Instances {
    def await: AwaitResult = new AwaitResult  {
      def result[A](f: Future[A], atMost: FiniteDuration): A = Await.result(f, atMost)
    }
  }
}
