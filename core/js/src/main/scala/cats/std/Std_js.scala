package cats
package std

import scala.concurrent.{Future}
import scala.concurrent.duration.FiniteDuration

private[std] object Platform {

  trait Instances {

    // JS has no concept of waiting, so this stub allows the abstract test classes to compile
    // but not run. Checkout Alleycats for an unlawful implementation
    def await: AwaitResult = new AwaitResult  {
      def result[A](f: Future[A], atMost: FiniteDuration): A = ???
    }
  }
}
