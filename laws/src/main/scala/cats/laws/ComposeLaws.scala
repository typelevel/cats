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

import cats.arrow.Compose
import cats.syntax.compose.*

/**
 * Laws that must be obeyed by any `cats.arrow.Compose`.
 */
trait ComposeLaws[F[_, _]] {
  implicit def F: Compose[F]

  def composeAssociativity[A, B, C, D](fab: F[A, B], fbc: F[B, C], fcd: F[C, D]): IsEq[F[A, D]] =
    (fab.andThen(fbc).andThen(fcd)) <-> (fab.andThen(fbc.andThen(fcd)))
}

object ComposeLaws {
  def apply[F[_, _]](implicit ev: Compose[F]): ComposeLaws[F] =
    new ComposeLaws[F] { def F: Compose[F] = ev }
}
