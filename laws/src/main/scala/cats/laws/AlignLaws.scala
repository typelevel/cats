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

import cats.syntax.align.*
import cats.syntax.functor.*

import cats.data.Ior
import cats.data.Ior.{Both, Left, Right}

/**
 * Laws that must be obeyed by any `Align`.
 */
trait AlignLaws[F[_]] {
  implicit def F: Align[F]

  implicit val functor: Functor[F] = F.functor

  def alignAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[Ior[Ior[A, B], C]]] =
    fa.align(fb).align(fc) <-> fa.align(fb.align(fc)).map(assoc)

  def alignHomomorphism[A, B, C, D](fa: F[A], fb: F[B], f: A => C, g: B => D): IsEq[F[Ior[C, D]]] =
    fa.map(f).align(fb.map(g)) <-> fa.align(fb).map(_.bimap(f, g))

  def alignWithConsistent[A, B, C](fa: F[A], fb: F[B], f: A Ior B => C): IsEq[F[C]] =
    fa.alignWith(fb)(f) <-> fa.align(fb).map(f)

  def alignMergeWithConsistent[A](fa1: F[A], fa2: F[A], f: (A, A) => A): IsEq[F[A]] =
    fa1.alignMergeWith(fa2)(f) <-> fa1.align(fa2).map(_.mergeWith(f))

  private def assoc[A, B, C](x: Ior[A, Ior[B, C]]): Ior[Ior[A, B], C] =
    x match {
      case Left(a) => Left(Left(a))
      case Right(bc) =>
        bc match {
          case Left(b)    => Left(Right(b))
          case Right(c)   => Right(c)
          case Both(b, c) => Both(Right(b), c)
        }
      case Both(a, bc) =>
        bc match {
          case Left(b)    => Left(Both(a, b))
          case Right(c)   => Both(Left(a), c)
          case Both(b, c) => Both(Both(a, b), c)
        }
    }
}

object AlignLaws {
  def apply[F[_]](implicit ev: Align[F]): AlignLaws[F] =
    new AlignLaws[F] { def F: Align[F] = ev }
}
