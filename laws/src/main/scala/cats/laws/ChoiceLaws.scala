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

import cats.arrow.Choice
import cats.syntax.choice.*
import cats.syntax.compose.*

/**
 * Laws that must be obeyed by any `cats.arrow.Choice`.
 */
trait ChoiceLaws[F[_, _]] extends CategoryLaws[F] {
  implicit override def F: Choice[F]

  def choiceCompositionDistributivity[A, B, C, D](fac: F[A, C], fbc: F[B, C], fcd: F[C, D]): IsEq[F[Either[A, B], D]] =
    ((fac ||| fbc) >>> fcd) <-> ((fac >>> fcd) ||| (fbc >>> fcd))
}

object ChoiceLaws {
  def apply[F[_, _]](implicit ev: Choice[F]): ChoiceLaws[F] =
    new ChoiceLaws[F] { def F: Choice[F] = ev }
}
