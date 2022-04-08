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

package cats.laws

import cats.CommutativeApply

/**
 * Laws that must be obeyed by any `CommutativeApply`.
 */
trait CommutativeApplyLaws[F[_]] extends ApplyLaws[F] {
  implicit override def F: CommutativeApply[F]

  def applyCommutative[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map2(fa, fb)(f) <-> F.map2(fb, fa)((b, a) => f(a, b))

}

object CommutativeApplyLaws {
  def apply[F[_]](implicit ev: CommutativeApply[F]): CommutativeApplyLaws[F] =
    new CommutativeApplyLaws[F] { def F: CommutativeApply[F] = ev }
}
