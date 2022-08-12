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
 * CommutativeSemigroup represents a commutative semigroup.
 *
 * A semigroup is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeSemigroup[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A] { self =>
  override def reverse: CommutativeSemigroup[A] = self
  override def intercalate(middle: A): CommutativeSemigroup[A] =
    new CommutativeSemigroup[A] {
      def combine(a: A, b: A): A =
        self.combine(a, self.combine(middle, b))

      override def combineN(a: A, n: Int): A =
        if (n <= 1) self.combineN(a, n)
        else {
          // a + m + a ... = combineN(a, n) + combineN(m, n - 1)
          self.combine(self.combineN(a, n), self.combineN(middle, n - 1))
        }
    }
}

object CommutativeSemigroup extends SemigroupFunctions[CommutativeSemigroup] {

  /**
   * Access an implicit `CommutativeSemigroup[A]`.
   */
  @inline final def apply[A](implicit ev: CommutativeSemigroup[A]): CommutativeSemigroup[A] = ev

  /**
   * Create a `CommutativeSemigroup` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): CommutativeSemigroup[A] = cmb(_, _)
}
