package cats.kernel
package instances

trait CharInstances {
  implicit val catsKernelStdOrderForChar: Order[Char] with Hash[Char] with BoundedEnum[Char] = new CharOrder
}

trait CharBounded extends BoundedEnum[Char] {
  override def minBound: Char = Char.MinValue
  override def maxBound: Char = Char.MaxValue
  override def partialNext(a: Char): Option[Char] =
    if(a == maxBound) None else Some((a + 1).toChar)
  override def partialPrevious(a: Char): Option[Char] =
    if (a == minBound) None else Some((a - 1).toChar)
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

  override val order: Order[Char] = self
}
