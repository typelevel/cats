package cats
package instances

import scala.concurrent.duration.FiniteDuration

trait FiniteDurationInstances extends cats.kernel.instances.FiniteDurationInstances {

  val catsStdShowForFiniteDuration: Show[FiniteDuration] =
    CoreDurationInstances.materialise.catsStdShowForFiniteDurationUnambiguous
}
