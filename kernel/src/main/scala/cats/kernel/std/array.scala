package cats.kernel
package std

import scala.{ specialized => sp }

package object array extends ArrayInstances

trait ArrayInstances {
  implicit def arrayEq[@sp A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]
  implicit def arrayOrder[@sp A: Order]: Order[Array[A]] =
    new ArrayOrder[A]
  implicit def arrayPartialOrder[@sp A: PartialOrder]: PartialOrder[Array[A]] =
    new ArrayPartialOrder[A]
}

object ArraySupport {

  private def signum(x: Int): Int =
    if(x < 0) -1
    else if(x > 0) 1
    else 0

  def eqv[@sp A: Eq](x: Array[A], y: Array[A]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && Eq.eqv(x(i), y(i))) i += 1
    i == x.length
  }

  def compare[@sp A: Order](x: Array[A], y: Array[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = Order.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    signum(x.length - y.length)
  }

  def partialCompare[@sp A: PartialOrder](x: Array[A], y: Array[A]): Double = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = PartialOrder.partialCompare(x(i), y(i))
      // Double.NaN is also != 0.0
      if (cmp != 0.0) return cmp
      i += 1
    }
    signum(x.length - y.length).toDouble
  }
}

final class ArrayEq[@sp A: Eq] extends Eq[Array[A]] {
  def eqv(x: Array[A], y: Array[A]): Boolean =
    ArraySupport.eqv(x, y)
}

final class ArrayOrder[@sp A: Order] extends Order[Array[A]] {
  override def eqv(x: Array[A], y: Array[A]): Boolean =
    ArraySupport.eqv(x, y)
  def compare(x: Array[A], y: Array[A]): Int =
    ArraySupport.compare(x, y)
}

final class ArrayPartialOrder[@sp A: PartialOrder] extends PartialOrder[Array[A]] {
  override def eqv(x: Array[A], y: Array[A]): Boolean =
    ArraySupport.eqv(x, y)
  override def partialCompare(x: Array[A], y: Array[A]): Double =
    ArraySupport.partialCompare(x, y)
}
