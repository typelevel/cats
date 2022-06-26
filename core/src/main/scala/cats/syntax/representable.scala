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
package syntax

trait RepresentableSyntax {
  implicit final def catsSyntaxTabulate[A, R](f: R => A): TabulateOps[A, R] =
    new TabulateOps[A, R](f)

  implicit final def catsSyntaxIndex[F[_], A](fa: F[A]): IndexOps[F, A] =
    new IndexOps[F, A](fa)
}

final class IndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def index[R](implicit R: Representable.Aux[F, R]): R => A = R.index(fa)
}

final class TabulateOps[A, R](private val f: R => A) extends AnyVal {
  def tabulate[F[_]](implicit R: Representable.Aux[F, R]): F[A] = R.tabulate(f)
}
