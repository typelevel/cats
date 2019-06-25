package cats
package instances

import java.util.UUID

trait UUIDInstances extends cats.kernel.instances.UUIDInstances {
  implicit val catsStdShowForUUID: Show[UUID] =
    Show.fromToString[UUID]
}

trait UUIDInstancesBinCompat0 extends cats.kernel.instances.UUIDInstancesBinCompat0
