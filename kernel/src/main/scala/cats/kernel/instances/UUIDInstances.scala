package cats.kernel
package instances

import java.util.UUID

trait UUIDInstances {
  implicit val catsKernelStdOrderForUUID: Order[UUID] with Hash[UUID] = new Order[UUID] with Hash[UUID] {
    def compare(x: UUID, y: UUID): Int = x.compareTo(y)
    def hash(x: UUID): Int = x.hashCode()
  }
}

trait UUIDInstancesBinCompat0 extends UUIDInstances {
  implicit val catsKernelStdBoundedForUUID: LowerBounded[UUID] with UpperBounded[UUID] =
    new LowerBounded[UUID] with UpperBounded[UUID] {
      override def minBound: UUID = new UUID(Long.MinValue, Long.MinValue)
      override def maxBound: UUID = new UUID(Long.MaxValue, Long.MaxValue)
      override val partialOrder: PartialOrder[UUID] = catsKernelStdOrderForUUID
    }
}
