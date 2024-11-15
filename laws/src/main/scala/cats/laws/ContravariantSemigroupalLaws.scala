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

import cats.ContravariantSemigroupal
import cats.syntax.contravariantSemigroupal.*

/**
 * Laws that are expected for any `cats.ContravariantSemigroupal`.
 */
trait ContravariantSemigroupalLaws[F[_]] extends ContravariantLaws[F] with SemigroupalLaws[F] {
  implicit override def F: ContravariantSemigroupal[F]

  def delta[A](a: A): (A, A) = (a, a)

  def contravariantSemigroupalContramap2DiagonalAssociates[A](m: F[A], n: F[A], o: F[A]): IsEq[F[A]] =
    ((m, n).contramapN(delta[A]), o).contramapN(delta[A]) <-> (m, (n, o).contramapN(delta[A])).contramapN(delta[A])
}
object ContravariantSemigroupalLaws {
  def apply[F[_]](implicit ev: ContravariantSemigroupal[F]): ContravariantSemigroupalLaws[F] =
    new ContravariantSemigroupalLaws[F] { def F: ContravariantSemigroupal[F] = ev }
}
