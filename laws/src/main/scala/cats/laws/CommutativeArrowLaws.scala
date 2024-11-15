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

import cats.arrow.CommutativeArrow
import cats.syntax.compose.*
import cats.syntax.strong.*

/**
 * Reference: "Causal Commutative Arrows", Journal of Functional Programming
 *  Figure 4.
 */
trait CommutativeArrowLaws[F[_, _]] extends ArrowLaws[F] {
  implicit override def F: CommutativeArrow[F]

  def arrowCommutative[A, B, C, D](f: F[A, B], g: F[C, D]): IsEq[F[(A, C), (B, D)]] =
    (f.first[C] >>> g.second[B]) <-> (g.second[A] >>> f.first[D])

}

object CommutativeArrowLaws {
  def apply[F[_, _]](implicit ev: CommutativeArrow[F]): CommutativeArrowLaws[F] =
    new CommutativeArrowLaws[F] { def F: CommutativeArrow[F] = ev }
}
