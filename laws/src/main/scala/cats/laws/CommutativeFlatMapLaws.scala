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

package cats
package laws

/**
 * Laws that must be obeyed by any `CommutativeFlatMap`.
 */
trait CommutativeFlatMapLaws[F[_]] extends CommutativeApplyLaws[F] with FlatMapLaws[F] {
  implicit override def F: CommutativeFlatMap[F]

  def flatmapCommutative[A, B, C](fa: F[A], fb: F[B], g: (A, B) => F[C]): IsEq[F[C]] =
    F.flatMap(fa)(a => F.flatMap(fb)(b => g(a, b))) <->
      F.flatMap(fb)(b => F.flatMap(fa)(a => g(a, b)))

}

object CommutativeFlatMapLaws {
  def apply[F[_]](implicit ev: CommutativeFlatMap[F]): CommutativeFlatMapLaws[F] =
    new CommutativeFlatMapLaws[F] { def F: CommutativeFlatMap[F] = ev }
}
