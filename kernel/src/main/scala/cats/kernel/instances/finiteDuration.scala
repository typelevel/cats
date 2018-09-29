package cats.kernel
package instances

import scala.concurrent.duration.Duration

import scala.concurrent.duration.FiniteDuration

package object finiteDuration extends FiniteDurationInstances

trait FiniteDurationInstances {
  implicit val catsKernelStdOrderForDuration: Order[FiniteDuration] with Hash[FiniteDuration] = new FiniteDurationOrder
  implicit val catsKernelStdGroupForDuration: CommutativeGroup[FiniteDuration] = new FiniteDurationGroup
}


/**
  * This ordering is valid for all defined durations.
  *
  * The value Duration.Undefined breaks our laws, because undefined
  * values are not equal to themselves.
  */
class FiniteDurationOrder extends Order[FiniteDuration] with Hash[FiniteDuration] {
  def hash(x: FiniteDuration): Int = x.hashCode()

  def compare(x: FiniteDuration, y: FiniteDuration): Int = x compare y

  override def eqv(x: FiniteDuration, y: FiniteDuration): Boolean = x == y
  override def neqv(x: FiniteDuration, y: FiniteDuration): Boolean = x != y
  override def gt(x: FiniteDuration, y: FiniteDuration): Boolean = x > y
  override def gteqv(x: FiniteDuration, y: FiniteDuration): Boolean = x >= y
  override def lt(x: FiniteDuration, y: FiniteDuration): Boolean = x < y
  override def lteqv(x: FiniteDuration, y: FiniteDuration): Boolean = x <= y

  override def min(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x min y
  override def max(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x max y
}

/**
  * This group models addition, but has a few problematic edge cases.
  *
  *   1. finite values can overflow, throwing an exception
  *   2. inf + (-inf) = undefined, not zero
  *   3. undefined + zero = undefined
  */
class FiniteDurationGroup extends CommutativeGroup[FiniteDuration] {
  def empty: FiniteDuration = Duration.Zero
  def inverse(x: FiniteDuration): FiniteDuration = -x
  def combine(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x + y
  override def remove(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x - y
}
