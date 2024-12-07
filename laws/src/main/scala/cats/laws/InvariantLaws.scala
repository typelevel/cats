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

import cats.Invariant
import cats.syntax.invariant.*

/**
 * Laws that must be obeyed by any `cats.Invariant`.
 */
trait InvariantLaws[F[_]] {
  implicit def F: Invariant[F]

  def invariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.imap(identity[A])(identity[A]) <-> fa

  def invariantComposition[A, B, C](fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B): IsEq[F[C]] =
    fa.imap(f1)(f2).imap(g1)(g2) <-> fa.imap(g1.compose(f1))(f2.compose(g2))
}

object InvariantLaws {
  def apply[F[_]](implicit ev: Invariant[F]): InvariantLaws[F] =
    new InvariantLaws[F] { def F: Invariant[F] = ev }
}
