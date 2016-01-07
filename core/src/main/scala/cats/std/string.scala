package cats
package std

trait StringInstances {
  implicit val stringOrder: Order[String] = new StringOrder
  implicit val stringMonoid = new StringMonoid
  implicit val stringShow: Show[String] =
    Show.fromToString[String]
}

class StringOrder extends Order[String] {
  def compare(x: String, y: String): Int = x compare y
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
