package cats.kernel
package instances

import scala.concurrent.duration.Duration

package object duration extends DurationInstances

trait DurationInstances {
  implicit val catsKernelStdOrderForDuration: Order[Duration] = new DurationOrder
  implicit val catsKernelStdGroupForDuration: CommutativeGroup[Duration] = new DurationGroup
}

class DurationOrder extends Order[Duration] {
  def compare(x: Duration, y: Duration): Int = x compare y

  override def eqv(x: Duration, y: Duration): Boolean = x == y
  override def neqv(x: Duration, y: Duration): Boolean = x != y
  override def gt(x: Duration, y: Duration): Boolean = x > y
  override def gteqv(x: Duration, y: Duration): Boolean = x >= y
  override def lt(x: Duration, y: Duration): Boolean = x < y
  override def lteqv(x: Duration, y: Duration): Boolean = x <= y

  override def min(x: Duration, y: Duration): Duration = x min y
  override def max(x: Duration, y: Duration): Duration = x max y
}

class DurationGroup extends CommutativeGroup[Duration] {
  def empty: Duration = Duration.Zero
  def inverse(x: Duration): Duration = -x
  def combine(x: Duration, y: Duration): Duration = x + y
  override def remove(x: Duration, y: Duration): Duration = x - y
}
