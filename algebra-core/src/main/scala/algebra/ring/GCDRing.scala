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

package algebra
package ring

import scala.{specialized => sp}

/**
 * GCDRing implements a GCD ring.
 *
 * For two elements x and y in a GCD ring, we can choose two elements d and m
 * such that:
 *
 * d = gcd(x, y)
 * m = lcm(x, y)
 *
 * d * m = x * y
 *
 * Additionally, we require:
 *
 * gcd(0, 0) = 0
 * lcm(x, 0) = lcm(0, x) = 0
 *
 * and commutativity:
 *
 * gcd(x, y) = gcd(y, x)
 * lcm(x, y) = lcm(y, x)
 */
trait GCDRing[@sp(Int, Long, Float, Double) A] extends Any with CommutativeRing[A] {
  def gcd(a: A, b: A)(implicit ev: Eq[A]): A
  def lcm(a: A, b: A)(implicit ev: Eq[A]): A
}

trait GCDRingFunctions[R[T] <: GCDRing[T]] extends RingFunctions[R] {
  def gcd[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A], eqA: Eq[A]): A =
    ev.gcd(a, b)(eqA)
  def lcm[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A], eqA: Eq[A]): A =
    ev.lcm(a, b)(eqA)
}

object GCDRing extends GCDRingFunctions[GCDRing] {
  @inline final def apply[A](implicit ev: GCDRing[A]): GCDRing[A] = ev
}
