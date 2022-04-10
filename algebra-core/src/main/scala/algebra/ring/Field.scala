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

trait Field[@sp(Int, Long, Float, Double) A]
    extends Any
    with EuclideanRing[A]
    with DivisionRing[A]
    with CommutativeSemifield[A] {
  self =>

  // default implementations for GCD

  override def gcd(a: A, b: A)(implicit eqA: Eq[A]): A =
    if (isZero(a) && isZero(b)) zero else one
  override def lcm(a: A, b: A)(implicit eqA: Eq[A]): A = times(a, b)

  // default implementations for Euclidean division in a field (as every nonzero element is a unit!)

  def euclideanFunction(a: A): BigInt = BigInt(0)
  def equot(a: A, b: A): A = div(a, b)
  def emod(a: A, b: A): A = zero
  override def equotmod(a: A, b: A): (A, A) = (div(a, b), zero)

  // needed for bin-compat
  override def fromDouble(a: Double): A =
    DivisionRing.defaultFromDouble[A](a)(self, self)

}

trait FieldFunctions[F[T] <: Field[T]] extends EuclideanRingFunctions[F] with MultiplicativeGroupFunctions[F] {
  def fromDouble[@sp(Int, Long, Float, Double) A](n: Double)(implicit ev: F[A]): A =
    ev.fromDouble(n)
}

object Field extends FieldFunctions[Field] {
  @inline final def apply[A](implicit ev: Field[A]): Field[A] = ev
}
