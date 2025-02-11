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

import cats.Bifunctor
import cats.syntax.bifunctor.*

/**
 * Laws that must be obeyed by any `Bifunctor`.
 */
trait BifunctorLaws[F[_, _]] {
  implicit def F: Bifunctor[F]

  def bifunctorIdentity[A, B](fa: F[A, B]): IsEq[F[A, B]] =
    fa.bimap(identity, identity) <-> fa

  def bifunctorComposition[A, B, C, X, Y, Z](fa: F[A, X], f: A => B, f2: B => C, g: X => Y, g2: Y => Z): IsEq[F[C, Z]] =
    fa.bimap(f, g).bimap(f2, g2) <-> fa.bimap(f.andThen(f2), g.andThen(g2))

  def bifunctorLeftMapIdentity[A, B](fa: F[A, B]): IsEq[F[A, B]] =
    fa.leftMap(identity) <-> fa

  def bifunctorLeftMapComposition[A, B, C, D](fa: F[A, B], f: A => C, g: C => D): IsEq[F[D, B]] =
    fa.leftMap(f).leftMap(g) <-> fa.leftMap(f.andThen(g))

}

object BifunctorLaws {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): BifunctorLaws[F] =
    new BifunctorLaws[F] {
      def F: Bifunctor[F] = ev
    }
}
