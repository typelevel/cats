/*
 * Copyright (c) 2022 Typelevel
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
package syntax

trait MonadSyntax {
  implicit final def catsSyntaxMonad[F[_], A](fa: F[A]): MonadOps[F, A] = new MonadOps(fa)

  implicit final def catsSyntaxMonadIdOps[A](a: A): MonadIdOps[A] =
    new MonadIdOps[A](a)
}

final class MonadIdOps[A](private val a: A) extends AnyVal {

  /**
   * Iterative application of `f` while `p` holds.
   */
  def iterateWhileM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhileM(a)(f)(p)

  /**
   * Iterative application of `f` until `p` holds.
   */
  def iterateUntilM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntilM(a)(f)(p)
}
