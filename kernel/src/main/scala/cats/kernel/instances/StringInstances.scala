package cats.kernel
package instances
import compat.scalaVersionSpecific._
@suppressUnusedImportWarningForScalaVersionSpecific
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

class StringMonoid extends Monoid[String] { self =>
  def empty: String = ""
  def combine(x: String, y: String): String = x + y

  override def combineAll(xs: IterableOnce[String]): String = {
    val sb = new StringBuilder
    xs.iterator.foreach(sb.append)
    sb.toString
  }

  override def reverse: Monoid[String] =
    new Monoid[String] {
      def empty = self.empty
      def combine(x: String, y: String) = y + x
      override def combineAll(xs: IterableOnce[String]): String = {
        val revStrings = xs.foldLeft(List.empty[String]) { (acc, s) => s :: acc }
        self.combineAll(revStrings)
      }

      override def reverse = self
    }
}
