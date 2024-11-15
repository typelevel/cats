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

import cats.syntax.all.*

trait NonEmptyAlternativeLaws[F[_]] extends ApplicativeLaws[F] with SemigroupKLaws[F] {
  implicit override def F: NonEmptyAlternative[F]
  implicit def algebra[A]: Semigroup[F[A]] = F.algebra[A]

  def nonEmptyAlternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    ((fa |+| fa2).map(f)) <-> ((fa.map(f)) |+| (fa2.map(f)))

  def nonEmptyAlternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    ((ff |+| fg).ap(fa)) <-> ((ff.ap(fa)) |+| (fg.ap(fa)))

  def nonEmptyAlternativePrependKConsitentWithPureAndCombineK[A](fa: F[A], a: A): IsEq[F[A]] =
    fa.prependK(a) <-> (a.pure[F] <+> fa)

  def nonEmptyAlternativeAppendKConsitentWithPureAndCombineK[A](fa: F[A], a: A): IsEq[F[A]] =
    fa.appendK(a) <-> (fa <+> a.pure[F])
}

object NonEmptyAlternativeLaws {
  def apply[F[_]](implicit ev: NonEmptyAlternative[F]): NonEmptyAlternativeLaws[F] =
    new NonEmptyAlternativeLaws[F] { def F: NonEmptyAlternative[F] = ev }

  def composed[M[_], N[_]](implicit
    M: NonEmptyAlternative[M],
    N: Applicative[N]
  ): NonEmptyAlternativeLaws[λ[α => M[N[α]]]] =
    new NonEmptyAlternativeLaws[λ[α => M[N[α]]]] {
      def F: NonEmptyAlternative[λ[α => M[N[α]]]] = M.compose[N]
    }
}
