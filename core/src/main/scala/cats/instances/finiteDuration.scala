package cats
package instances

import scala.concurrent.duration.FiniteDuration

trait FiniteDurationInstances extends cats.kernel.instances.FiniteDurationInstances {
  implicit val catsStdShowForFiniteDuration: Show[FiniteDuration] =
    Show.fromToString[FiniteDuration]
}
