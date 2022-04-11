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
 * Laws that must be obeyed by any `cats.SemigroupK`.
 */
trait SemigroupKLaws[F[_]] {
  implicit def F: SemigroupK[F]

  def semigroupKAssociative[A](a: F[A], b: F[A], c: F[A]): IsEq[F[A]] =
    F.combineK(F.combineK(a, b), c) <-> F.combineK(a, F.combineK(b, c))

  def repeat1K[A](a: F[A]): IsEq[F[A]] =
    F.combineNK(a, 1) <-> a

  def repeat2K[A](a: F[A]): IsEq[F[A]] =
    F.combineNK(a, 2) <-> F.combineK(a, a)

  def combineAllOptionK[A](xs: Vector[F[A]]): IsEq[Option[F[A]]] =
    F.combineAllOptionK(xs) <-> xs.reduceOption(F.combineK[A])

  def reverseReversesK[A](a: F[A], b: F[A]): IsEq[F[A]] =
    F.combineK(a, b) <-> F.reverse.combineK(b, a)

  def reverseRepeat1K[A](a: F[A]): IsEq[F[A]] = {
    val rev = F.reverse
    rev.combineNK(a, 1) <-> a
  }

  def reverseRepeat2K[A](a: F[A]): IsEq[F[A]] = {
    val rev = F.reverse
    rev.combineNK(a, 2) <-> rev.combineK(a, a)
  }

  def reverseCombineAllOptionK[A](xs: Vector[F[A]]): IsEq[Option[F[A]]] = {
    val rev = F.reverse
    rev.combineAllOptionK(xs) <-> xs.reduceOption(rev.combineK[A])
  }

}

object SemigroupKLaws {
  def apply[F[_]](implicit ev: SemigroupK[F]): SemigroupKLaws[F] =
    new SemigroupKLaws[F] { def F: SemigroupK[F] = ev }
}
