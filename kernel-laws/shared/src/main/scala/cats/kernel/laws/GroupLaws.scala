/*
 * Copyright (c) 2022 Typelevel
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

trait GroupLaws[A] extends MonoidLaws[A] {
  implicit override def S: Group[A]

  def leftInverse(x: A): IsEq[A] =
    S.empty <-> S.combine(S.inverse(x), x)

  def rightInverse(x: A): IsEq[A] =
    S.empty <-> S.combine(x, S.inverse(x))

  def consistentInverse(x: A, y: A): IsEq[A] =
    S.remove(x, y) <-> S.combine(x, S.inverse(y))
}

object GroupLaws {
  def apply[A](implicit ev: Group[A]): GroupLaws[A] =
    new GroupLaws[A] { def S: Group[A] = ev }
}
