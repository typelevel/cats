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

import cats.data.Cokleisli
import cats.syntax.all.*

/**
 * Laws that must be obeyed by any `Comonad`.
 */
trait ComonadLaws[F[_]] extends CoflatMapLaws[F] {
  implicit override def F: Comonad[F]

  def extractCoflattenIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.coflatten.extract <-> fa

  def mapCoflattenIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.coflatten.map(_.extract) <-> fa

  def mapCoflatMapCoherence[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> fa.coflatMap(fa0 => f(fa0.extract))

  def comonadLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.coflatMap(_.extract) <-> fa

  def comonadRightIdentity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    fa.coflatMap(f).extract <-> f(fa)

  /**
   * `extract` is the left identity element under left-to-right composition of
   * `cats.data.Cokleisli` arrows. This is analogous to [[comonadLeftIdentity]].
   */
  def cokleisliLeftIdentity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    Cokleisli(F.extract[A]).andThen(Cokleisli(f)).run(fa) <-> f(fa)

  /**
   * `extract` is the right identity element under left-to-right composition of
   * `cats.data.Cokleisli` arrows. This is analogous to [[comonadRightIdentity]].
   */
  def cokleisliRightIdentity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    Cokleisli(f).andThen(Cokleisli(F.extract[B])).run(fa) <-> f(fa)
}

object ComonadLaws {
  def apply[F[_]](implicit ev: Comonad[F]): ComonadLaws[F] =
    new ComonadLaws[F] { def F: Comonad[F] = ev }
}
