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
package ring

import scala.{specialized => sp}

trait DivisionRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Ring[A] with Semifield[A] {
  self =>

  /**
   * This is implemented in terms of basic Ring ops. However, this is
   * probably significantly less efficient than can be done with a
   * specific type. So, it is recommended that this method be
   * overridden.
   *
   * This is possible because a Double is a rational number.
   */
  def fromDouble(a: Double): A = DivisionRing.defaultFromDouble[A](a)(self, self)

}

trait DivisionRingFunctions[F[T] <: DivisionRing[T]] extends RingFunctions[F] with MultiplicativeGroupFunctions[F] {
  def fromDouble[@sp(Int, Long, Float, Double) A](n: Double)(implicit ev: F[A]): A =
    ev.fromDouble(n)
}

object DivisionRing extends DivisionRingFunctions[DivisionRing] {

  @inline final def apply[A](implicit f: DivisionRing[A]): DivisionRing[A] = f

}
