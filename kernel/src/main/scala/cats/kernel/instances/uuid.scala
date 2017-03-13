package cats.kernel
package instances

import java.util.UUID

package object uuid extends UUIDInstances

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] =
    Order.fromComparable[UUID]
}
