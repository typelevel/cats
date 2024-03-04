/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
        val revStrings = xs.iterator.foldLeft(List.empty[String]) { (acc, s) =>
          s :: acc
        }
        self.combineAll(revStrings)
      }

      override def reverse: Monoid[String] = self
    }
}
