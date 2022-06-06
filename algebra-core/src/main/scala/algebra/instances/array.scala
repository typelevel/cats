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

package algebra
package instances

import scala.{specialized => sp}

package object array extends ArrayInstances

trait ArrayInstances {
  implicit def arrayEq[@sp A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]
  implicit def arrayOrder[@sp A: Order]: Order[Array[A]] =
    new ArrayOrder[A]
  implicit def arrayPartialOrder[@sp A: PartialOrder]: PartialOrder[Array[A]] =
    new ArrayPartialOrder[A]
}

private object ArraySupport {

  private def signum(x: Int): Int =
    if (x < 0) -1
    else if (x > 0) 1
    else 0

  def eqv[@sp A](x: Array[A], y: Array[A])(implicit ev: Eq[A]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && ev.eqv(x(i), y(i))) i += 1
    i == x.length
  }

  def compare[@sp A](x: Array[A], y: Array[A])(implicit ev: Order[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = ev.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    signum(x.length - y.length)
  }

  def partialCompare[@sp A](x: Array[A], y: Array[A])(implicit ev: PartialOrder[A]): Double = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = ev.partialCompare(x(i), y(i))
      // Double.NaN is also != 0.0
      if (cmp != 0.0) return cmp
      i += 1
    }
    signum(x.length - y.length).toDouble
  }
}

final private class ArrayEq[@sp A: Eq] extends Eq[Array[A]] with Serializable {
  def eqv(x: Array[A], y: Array[A]): Boolean =
    ArraySupport.eqv(x, y)
}

final private class ArrayOrder[@sp A: Order] extends Order[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean =
    ArraySupport.eqv(x, y)
  def compare(x: Array[A], y: Array[A]): Int =
    ArraySupport.compare(x, y)
}

final private class ArrayPartialOrder[@sp A: PartialOrder] extends PartialOrder[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean =
    ArraySupport.eqv(x, y)
  override def partialCompare(x: Array[A], y: Array[A]): Double =
    ArraySupport.partialCompare(x, y)
}
