package cats.kernel
package instances

trait CharInstances {
  implicit val catsKernelStdOrderForChar: CharOrder = new CharOrder
}

trait CharBounded extends LowerBounded[Char] with UpperBounded[Char] {
  override def minBound: Char = Char.MinValue
  override def maxBound: Char = Char.MaxValue
}

class CharOrder extends Order[Char] with Hash[Char] with CharBounded { self =>
  def hash(x: Char): Int = x.hashCode()
  def compare(x: Char, y: Char): Int =
    if (x < y) -1 else if (x > y) 1 else 0
  override def eqv(x: Char, y: Char): Boolean = x == y
  override def neqv(x: Char, y: Char): Boolean = x != y
  override def gt(x: Char, y: Char): Boolean = x > y
  override def gteqv(x: Char, y: Char): Boolean = x >= y
  override def lt(x: Char, y: Char): Boolean = x < y
  override def lteqv(x: Char, y: Char): Boolean = x <= y

  override val partialOrder: PartialOrder[Char] = self
}
