package cats
package instances

import scala.concurrent.duration.Duration

trait DurationInstances extends cats.kernel.instances.DurationInstances {
  implicit val catsStdShowForDuration: Show[Duration] =
    Show.fromToString[Duration]
}
