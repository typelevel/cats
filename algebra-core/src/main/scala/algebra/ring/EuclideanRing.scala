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

import scala.annotation.tailrec
import scala.{specialized => sp}

/**
 * EuclideanRing implements a Euclidean domain.
 *
 * The formal definition says that every euclidean domain A has (at
 * least one) euclidean function f: A -> N (the natural numbers) where:
 *
 *   (for every x and non-zero y) x = yq + r, and r = 0 or f(r) < f(y).
 *
 * This generalizes the Euclidean division of integers, where f represents
 * a measure of length (or absolute value), and the previous equation
 * represents finding the quotient and remainder of x and y. So:
 *
 *   quot(x, y) = q
 *   mod(x, y) = r
 */
trait EuclideanRing[@sp(Int, Long, Float, Double) A] extends Any with GCDRing[A] { self =>
  def euclideanFunction(a: A): BigInt
  def equot(a: A, b: A): A
  def emod(a: A, b: A): A
  def equotmod(a: A, b: A): (A, A) = (equot(a, b), emod(a, b))
  def gcd(a: A, b: A)(implicit ev: Eq[A]): A =
    EuclideanRing.euclid(a, b)(using ev, self)
  def lcm(a: A, b: A)(implicit ev: Eq[A]): A =
    if (isZero(a) || isZero(b)) zero else times(equot(a, gcd(a, b)), b)
}

trait EuclideanRingFunctions[R[T] <: EuclideanRing[T]] extends GCDRingFunctions[R] {
  def euclideanFunction[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: R[A]): BigInt =
    ev.euclideanFunction(a)
  def equot[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): A =
    ev.equot(a, b)
  def emod[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): A =
    ev.emod(a, b)
  def equotmod[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): (A, A) =
    ev.equotmod(a, b)
}

object EuclideanRing extends EuclideanRingFunctions[EuclideanRing] {

  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e

  /**
   * Simple implementation of Euclid's algorithm for gcd
   */
  @tailrec final def euclid[@sp(Int, Long, Float, Double) A: Eq: EuclideanRing](a: A, b: A): A = {
    if (EuclideanRing[A].isZero(b)) a else euclid(b, EuclideanRing[A].emod(a, b))
  }

}
