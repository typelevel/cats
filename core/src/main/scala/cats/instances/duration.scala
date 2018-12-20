package cats
package instances

import scala.concurrent.duration.{Duration, FiniteDuration}

trait DurationInstances extends cats.kernel.instances.DurationInstances {

  @deprecated("Left to keep binary compatibility. Use CoreDurationInstances.catsStdShowForDurationUnambiguous instead.",
              "1.5.0")
  val catsStdShowForDuration: Show[Duration] =
    AllCoreDurationInstances.materialise.catsStdShowForDurationUnambiguous
}

private[instances] trait AllCoreDurationInstances extends CoreFiniteDurationInstances

private[instances] trait CoreFiniteDurationInstances extends CoreDurationInstances {
  implicit final val catsStdShowForFiniteDurationUnambiguous: Show[FiniteDuration] =
    Show.fromToString[FiniteDuration]
}

private[instances] trait CoreDurationInstances {
  implicit final val catsStdShowForDurationUnambiguous: Show[Duration] =
    Show.fromToString[Duration]
}

private[instances] object AllCoreDurationInstances {
  final val materialise: AllCoreDurationInstances = new AllCoreDurationInstances {}
}
