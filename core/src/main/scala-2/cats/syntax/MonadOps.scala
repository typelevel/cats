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

package cats.syntax

import cats.{Alternative, Monad}

final class MonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def whileM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntil(fa)(p)
  def whenM(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.whenM(fa)(p)
  def unlessM(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.unlessM(fa)(p)
  def flatMapOrKeep[A1 >: A](pfa: PartialFunction[A, F[A1]])(implicit M: Monad[F]): F[A1] =
    M.flatMapOrKeep[A, A1](fa)(pfa)
}
