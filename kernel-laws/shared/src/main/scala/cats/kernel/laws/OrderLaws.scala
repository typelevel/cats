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
package laws

trait OrderLaws[A] extends PartialOrderLaws[A] {

  implicit override def E: Order[A]

  def totality(x: A, y: A): IsEq[Boolean] =
    (E.lteqv(x, y) || E.lteqv(y, x)) <-> true

  def compare(x: A, y: A): IsEq[Boolean] = {
    val c = E.compare(x, y)
    (((c < 0) == E.lt(x, y)) && ((c == 0) == E.eqv(x, y)) && ((c > 0) == E.gt(x, y))) <-> true
  }

  def min(x: A, y: A): IsEq[Boolean] = {
    val c = E.compare(x, y)
    val m = E.min(x, y)
    if (c < 0) E.eqv(m, x) <-> true
    else if (c == 0) (E.eqv(m, x) && (E.eqv(m, y))) <-> true
    else E.eqv(m, y) <-> true
  }

  def max(x: A, y: A): IsEq[Boolean] = {
    val c = E.compare(x, y)
    val m = E.max(x, y)
    if (c < 0) E.eqv(m, y) <-> true
    else if (c == 0) (E.eqv(m, x) && (E.eqv(m, y))) <-> true
    else E.eqv(m, x) <-> true
  }

}

object OrderLaws {
  def apply[A](implicit ev: Order[A]): OrderLaws[A] =
    new OrderLaws[A] { def E: Order[A] = ev }
}
