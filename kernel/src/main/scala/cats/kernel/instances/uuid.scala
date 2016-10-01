package cats.kernel
package instances

import java.util.UUID

package object uuid extends UUIDInstances

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] = new UUIDOrder
}

class UUIDOrder extends Order[UUID] {

  override def eqv(x: UUID, y: UUID): Boolean =
    x == y

  def compare(x: UUID, y: UUID): Int =
    if (x eq y) 0 else x compareTo y
}
