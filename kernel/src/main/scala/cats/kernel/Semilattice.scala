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

import scala.{specialized => sp}

/**
 * Semilattices are commutative semigroups whose operation
 * (i.e. combine) is also idempotent.
 */
trait Semilattice[@sp(Int, Long, Float, Double) A] extends Any with Band[A] with CommutativeSemigroup[A] { self =>

  /**
   * Given Eq[A], return a PartialOrder[A] using the `combine`
   * operator to determine the partial ordering. This method assumes
   * `combine` functions as `meet` (that is, as a lower bound).
   *
   * This method returns:
   *
   *    0.0 if x = y
   *   -1.0 if x = combine(x, y)
   *    1.0 if y = combine(x, y)
   *    NaN otherwise
   */
  def asMeetPartialOrder(implicit ev: Eq[A]): PartialOrder[A] = (x, y) =>
    if (ev.eqv(x, y)) 0.0
    else {
      val z = self.combine(x, y)
      if (ev.eqv(x, z)) -1.0 else if (ev.eqv(y, z)) 1.0 else Double.NaN
    }

  /**
   * Given Eq[A], return a PartialOrder[A] using the `combine`
   * operator to determine the partial ordering. This method assumes
   * `combine` functions as `join` (that is, as an upper bound).
   *
   * This method returns:
   *
   *    0.0 if x = y
   *   -1.0 if y = combine(x, y)
   *    1.0 if x = combine(x, y)
   *    NaN otherwise
   */
  def asJoinPartialOrder(implicit ev: Eq[A]): PartialOrder[A] = (x, y) =>
    if (ev.eqv(x, y)) 0.0
    else {
      val z = self.combine(x, y)
      if (ev.eqv(y, z)) -1.0 else if (ev.eqv(x, z)) 1.0 else Double.NaN
    }
}

abstract class SemilatticeFunctions[S[T] <: Semilattice[T]] extends SemigroupFunctions[S] {
  def asMeetPartialOrder[A](implicit s: S[A], ev: Eq[A]): PartialOrder[A] =
    s.asMeetPartialOrder(ev)
  def asJoinPartialOrder[A](implicit s: S[A], ev: Eq[A]): PartialOrder[A] =
    s.asJoinPartialOrder(ev)
}

object Semilattice extends SemilatticeFunctions[Semilattice] {

  /**
   * Access an implicit `Semilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: Semilattice[A]): Semilattice[A] = ev

  /**
   * Create a `Semilattice` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): Semilattice[A] = cmb(_, _)
}
