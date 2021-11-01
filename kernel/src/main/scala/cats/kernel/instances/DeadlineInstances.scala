package cats.kernel
package instances

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Deadline, FiniteDuration}

trait DeadlineInstances {
  implicit val catsKernelStdOrderForDeadline
    : Order[Deadline] with Hash[Deadline] with LowerBounded[Deadline] with UpperBounded[Deadline] = new DeadlineOrder
}

trait DeadlineBounded extends LowerBounded[Deadline] with UpperBounded[Deadline] {
  override def minBound: Deadline = Deadline(FiniteDuration(-Long.MaxValue, TimeUnit.NANOSECONDS))
  override def maxBound: Deadline = Deadline(FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS))
}

class DeadlineOrder extends Order[Deadline] with Hash[Deadline] with DeadlineBounded { self =>

  def hash(x: Deadline): Int = x.hashCode()
  def compare(x: Deadline, y: Deadline): Int =
    if (x == y) 0 else if (x > y) 1 else -1

  override def eqv(x: Deadline, y: Deadline): Boolean = x == y
  override def neqv(x: Deadline, y: Deadline): Boolean = x != y
  override def gt(x: Deadline, y: Deadline): Boolean = x > y && x != y
  override def lt(x: Deadline, y: Deadline): Boolean = x < y && x != y
  override def gteqv(x: Deadline, y: Deadline): Boolean = x == y || x > y
  override def lteqv(x: Deadline, y: Deadline): Boolean = x == y || y > x

  override def min(x: Deadline, y: Deadline): Deadline = if (x < y) x else y
  override def max(x: Deadline, y: Deadline): Deadline = if (x > y) x else y

  override val partialOrder: PartialOrder[Deadline] = self
}
