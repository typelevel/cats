package cats.kernel
package instances

import java.util.UUID

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] with Hash[UUID] with LowerBounded[UUID] with UpperBounded[UUID] =
    new Order.FromComparable[UUID] with Hash.FromUniversal[UUID] with UUIDBounded { self =>
      val partialOrder: PartialOrder[UUID] = self
    }
}

trait UUIDBounded extends LowerBounded[UUID] with UpperBounded[UUID] {
  override def minBound: UUID = new UUID(Long.MinValue, Long.MinValue)
  override def maxBound: UUID = new UUID(Long.MaxValue, Long.MaxValue)
}
