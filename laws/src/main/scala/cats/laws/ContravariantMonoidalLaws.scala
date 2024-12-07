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

import cats.ContravariantMonoidal
import cats.syntax.contravariant.*
import cats.syntax.contravariantSemigroupal.*

/**
 * Laws that must hold for any `cats.ContravariantMonoidal`.
 */
trait ContravariantMonoidalLaws[F[_]] extends ContravariantSemigroupalLaws[F] {
  implicit override def F: ContravariantMonoidal[F]

  def contravariantMonoidalUnitRight[A](fa: F[A]): IsEq[F[A]] =
    (fa, F.trivial[A]).contramapN(delta[A]) <-> fa

  def contravariantMonoidalUnitLeft[A](fa: F[A]): IsEq[F[A]] =
    (F.trivial[A], fa).contramapN(delta[A]) <-> fa

  def contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C](fa: F[A], f: B => (A, C)): IsEq[F[B]] =
    (fa, F.trivial[C]).contramapN(f) <-> fa.contramap(f.andThen(_._1))

  def contravariantMonoidalContramap2CompatibleContramapRight[A, B, C](fa: F[A], f: C => (B, A)): IsEq[F[C]] =
    (F.trivial[B], fa).contramapN(f) <-> fa.contramap(f.andThen(_._2))
}

object ContravariantMonoidalLaws {
  def apply[F[_]](implicit ev: ContravariantMonoidal[F]): ContravariantMonoidalLaws[F] =
    new ContravariantMonoidalLaws[F] { def F: ContravariantMonoidal[F] = ev }
}
