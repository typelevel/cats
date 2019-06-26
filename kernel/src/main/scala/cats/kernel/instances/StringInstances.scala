package cats.kernel
package instances

trait StringInstances {
  implicit val catsKernelStdOrderForString: Order[String] with Hash[String] with LowerBounded[String] = new StringOrder
  implicit val catsKernelStdMonoidForString: Monoid[String] = new StringMonoid
}

trait StringLowerBounded extends LowerBounded[String] {
  override def minBound: String = ""
}

class StringOrder extends Order[String] with Hash[String] with StringLowerBounded { self =>

  def hash(x: String): Int = x.hashCode()

  override def eqv(x: String, y: String): Boolean =
    x == y
  def compare(x: String, y: String): Int =
    if (x eq y) 0 else x.compareTo(y)

  override val partialOrder: PartialOrder[String] = self
}

class StringMonoid extends Monoid[String] {
  def empty: String = ""
  def combine(x: String, y: String): String = x + y

  override def combineAll(xs: TraversableOnce[String]): String = {
    val sb = new StringBuilder
    xs.foreach(sb.append)
    sb.toString
  }
}
