package cats.kernel
package instances

package object string extends StringInstances

trait StringInstances {
  implicit val catsKernelStdOrderForString: Order[String] with Hash[String] = new StringOrder
  implicit val catsKernelStdMonoidForString: Monoid[String] = new StringMonoid
}

class StringOrder extends Order[String] with Hash[String] {

  def hash(x: String): Int = x.hashCode()

  override def eqv(x: String, y: String): Boolean =
    x == y
  def compare(x: String, y: String): Int =
    if (x eq y) 0 else x compareTo y
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
