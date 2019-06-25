package cats.kernel
package instances

import scala.concurrent.duration.Duration

trait DurationInstances {
  implicit val catsKernelStdOrderForDuration: Order[Duration] with Hash[Duration] =
    new DurationOrder
  implicit val catsKernelStdGroupForDuration: CommutativeGroup[Duration] = new DurationGroup
  implicit val catsKernelStdBoundedForDuration: LowerBounded[Duration] with UpperBounded[Duration] =
    new DurationBounded {
      override val partialOrder: PartialOrder[Duration] = catsKernelStdOrderForDuration
    }
}

// Duration.Undefined, Duration.Inf, Duration.MinusInf

/**
 * This ordering is valid for all defined durations.
 *
 * The value Duration.Undefined breaks our laws, because undefined
 * values are not equal to themselves.
 */
class DurationOrder extends Order[Duration] with Hash[Duration] {
  def hash(x: Duration): Int = x.hashCode()

  def compare(x: Duration, y: Duration): Int = x.compare(y)

  override def eqv(x: Duration, y: Duration): Boolean = x == y
  override def neqv(x: Duration, y: Duration): Boolean = x != y
  override def gt(x: Duration, y: Duration): Boolean = x > y
  override def gteqv(x: Duration, y: Duration): Boolean = x >= y
  override def lt(x: Duration, y: Duration): Boolean = x < y
  override def lteqv(x: Duration, y: Duration): Boolean = x <= y

  override def min(x: Duration, y: Duration): Duration = x.min(y)
  override def max(x: Duration, y: Duration): Duration = x.max(y)
}

/**
 * This group models addition, but has a few problematic edge cases.
 *
 *   1. finite values can overflow, throwing an exception
 *   2. inf + (-inf) = undefined, not zero
 *   3. undefined + zero = undefined
 */
class DurationGroup extends CommutativeGroup[Duration] {
  def empty: Duration = Duration.Zero
  def inverse(x: Duration): Duration = -x
  def combine(x: Duration, y: Duration): Duration = x + y
  override def remove(x: Duration, y: Duration): Duration = x - y
}

trait DurationBounded extends LowerBounded[Duration] with UpperBounded[Duration] {
  override def minBound: Duration = Duration.MinusInf
  override def maxBound: Duration = Duration.Inf
}
