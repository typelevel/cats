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

import cats.data.Kleisli
import cats.syntax.all._

/**
 * Laws that must be obeyed by any `Monad`.
 */
trait MonadLaws[F[_]] extends ApplicativeLaws[F] with FlatMapLaws[F] {
  implicit override def F: Monad[F]

  def monadLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    F.pure(a).flatMap(f) <-> f(a)

  def monadRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap(F.pure) <-> fa

  /**
   * `pure` is the left identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadLeftIdentity]].
   */
  def kleisliLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    Kleisli(F.pure[A]).andThen(Kleisli(f)).run(a) <-> f(a)

  /**
   * `pure` is the right identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadRightIdentity]].
   */
  def kleisliRightIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    Kleisli(f).andThen(Kleisli(F.pure[B])).run(a) <-> f(a)

  /**
   * Make sure that map and flatMap are consistent.
   */
  def mapFlatMapCoherence[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.flatMap(a => F.pure(f(a))) <-> fa.map(f)

  lazy val tailRecMStackSafety: IsEq[F[Int]] = {
    val n = 50000
    val res = F.tailRecM(0)(i => F.pure(if (i < n) Either.left(i + 1) else Either.right(i)))
    res <-> F.pure(n)
  }
}

object MonadLaws {
  def apply[F[_]](implicit ev: Monad[F]): MonadLaws[F] =
    new MonadLaws[F] { def F: Monad[F] = ev }
}
