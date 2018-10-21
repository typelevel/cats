package cats
package laws
package discipline

import cats.instances.int._

/**
 * Similar to `Int`, but with a much smaller domain. The exact range of [[MiniInt]] may be tuned
 * from time to time, so consumers of this type should avoid depending on its exact range.
 */
final class MiniInt private (val asInt: Int) extends AnyVal with Serializable

object MiniInt {
  val minIntValue: Int = -7
  val maxIntValue: Int = 7

  def isInDomain(i: Int): Boolean = i >= minIntValue && i <= maxIntValue

  def apply(i: Int): Option[MiniInt] = if (isInDomain(i)) Some(new MiniInt(i)) else None

  def unsafeApply(i: Int): MiniInt =
    if (isInDomain(i)) new MiniInt(i)
    else throw new IllegalArgumentException(s"Expected value between $minIntValue and $maxIntValue but got $i")

  val allValues: Stream[MiniInt] = (minIntValue to maxIntValue).map(unsafeApply).toStream

  implicit val catsLawsEqInstancesForMiniInt: Order[MiniInt] with Hash[MiniInt] = new Order[MiniInt]
  with Hash[MiniInt] {
    def hash(x: MiniInt): Int = Hash[Int].hash(x.asInt)

    def compare(x: MiniInt, y: MiniInt): Int = Order[Int].compare(x.asInt, y.asInt)
  }

  implicit val catsLawsExhuastiveCheckForMiniInt: ExhaustiveCheck[MiniInt] =
    ExhaustiveCheck.instance(allValues)
}
