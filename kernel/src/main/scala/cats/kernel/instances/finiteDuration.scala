package cats.kernel
package instances

import scala.concurrent.duration.{ Duration, FiniteDuration }


trait FiniteDurationInstances {
  implicit val catsKernelStdOrderForFiniteDuration: Order[FiniteDuration] with Hash[FiniteDuration] = new FiniteDurationOrder
  implicit val catsKernelStdGroupForFiniteDuration: CommutativeGroup[FiniteDuration] = new FiniteDurationGroup
}
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

class FiniteDurationGroup extends CommutativeGroup[FiniteDuration] {
  def empty: FiniteDuration = Duration.Zero
  def inverse(x: FiniteDuration): FiniteDuration = -x
  def combine(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x + y
  override def remove(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x - y
}
