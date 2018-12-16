package cats
package instances

import scala.concurrent.duration.{Duration, FiniteDuration}

trait DurationInstances extends cats.kernel.instances.DurationInstances {

  val catsStdShowForDuration: Show[Duration] =
    CoreDurationInstances.materialise.catsStdShowForDurationUnambiguous
}

private[instances] trait CoreDurationInstances extends CoreDurationInstances0

private[instances] sealed trait CoreDurationInstances0 extends CoreDurationInstances1 {
  implicit final val catsStdShowForFiniteDurationUnambiguous: Show[FiniteDuration] =
    Show.fromToString[FiniteDuration]
}

private[instances] sealed trait CoreDurationInstances1 {
  implicit final val catsStdShowForDurationUnambiguous: Show[Duration] =
    Show.fromToString[Duration]
}

private[instances] object CoreDurationInstances {
  final val materialise: CoreDurationInstances = new CoreDurationInstances {}
}
