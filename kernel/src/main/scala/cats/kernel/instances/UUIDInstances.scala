package cats.kernel
package instances

import java.util.UUID

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] with Hash[UUID] with LowerBounded[UUID] with UpperBounded[UUID] =
    new Order[UUID] with Hash[UUID] with UUIDBounded { self =>
      def compare(x: UUID, y: UUID): Int = x.compareTo(y)
      def hash(x: UUID): Int = x.hashCode()
      val partialOrder: PartialOrder[UUID] = self
    }
}

trait UUIDBounded extends LowerBounded[UUID] with UpperBounded[UUID] {
  override def minBound: UUID = new UUID(Long.MinValue, Long.MinValue)
  override def maxBound: UUID = new UUID(Long.MaxValue, Long.MaxValue)
}
