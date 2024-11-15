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
 * Laws that are expected for any `cats.InvariantSemigroupal`.
 */
trait InvariantSemigroupalLaws[F[_]] extends InvariantLaws[F] with SemigroupalLaws[F] {
  implicit override def F: InvariantSemigroupal[F]
  import cats.syntax.semigroupal.*
  import cats.syntax.invariant.*

  def invariantSemigroupalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, (B, C))]] =
    fa.product(fb.product(fc)) <-> fa
      .product(fb)
      .product(fc)
      .imap { case ((a, b), c) => (a, (b, c)) } { case (a, (b, c)) => ((a, b), c) }
}

object InvariantSemigroupalLaws {
  def apply[F[_]](implicit ev: InvariantSemigroupal[F]): InvariantSemigroupalLaws[F] =
    new InvariantSemigroupalLaws[F] { def F: InvariantSemigroupal[F] = ev }
}
