package cats
package instances

import scala.concurrent.duration.FiniteDuration

trait FiniteDurationInstances extends cats.kernel.instances.FiniteDurationInstances {

  @deprecated("Left to keep binary compatibility. Use CoreFiniteDurationInstances.catsStdShowForFiniteDurationUnambiguous instead.", "1.5.0")
  val catsStdShowForFiniteDuration: Show[FiniteDuration] =
    AllCoreDurationInstances.materialise.catsStdShowForFiniteDurationUnambiguous
}
