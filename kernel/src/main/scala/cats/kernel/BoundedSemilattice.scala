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

trait BoundedSemilattice[@sp(Int, Long, Float, Double) A] extends Any with Semilattice[A] with CommutativeMonoid[A] {
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else a // combine(a, a) == a for a semilattice
}

object BoundedSemilattice extends SemilatticeFunctions[BoundedSemilattice] {

  /**
   * Access an implicit `BoundedSemilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: BoundedSemilattice[A]): BoundedSemilattice[A] =
    ev

  /**
   * Create a `BoundedSemilattice` instance from the given function and empty value.
   */
  @inline def instance[A](emptyValue: A, cmb: (A, A) => A): BoundedSemilattice[A] =
    new BoundedSemilattice[A] {
      override val empty: A = emptyValue
      override def combine(x: A, y: A): A = cmb(x, y)
    }
}
