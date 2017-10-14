package cats.kernel
package instances

import java.util.UUID

package object uuid extends UUIDInstances

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] with Hash[UUID] = new Order[UUID] with Hash[UUID] {
    def compare(x: UUID, y: UUID): Int = x compareTo y
    def hash(x: UUID): Int = x.hashCode()
  }
}
