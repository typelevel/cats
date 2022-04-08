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

/**
 * Laws that must be obeyed by any `Bimonad`.
 *
 * For more information, see definition 4.1 from this paper:
 * http://arxiv.org/pdf/0710.1163v3.pdf
 */
trait BimonadLaws[F[_]] extends MonadLaws[F] with ComonadLaws[F] {
  implicit override def F: Bimonad[F]

  def pureExtractIsId[A](a: A): IsEq[A] =
    F.extract(F.pure(a)) <-> a

  def extractFlatMapEntwining[A](ffa: F[F[A]]): IsEq[A] =
    F.extract(F.flatten(ffa)) <-> F.extract(F.map(ffa)(F.extract))

  def pureCoflatMapEntwining[A](a: A): IsEq[F[F[A]]] =
    F.coflatten(F.pure(a)) <-> F.map(F.pure(a))(F.pure)
}

object BimonadLaws {
  def apply[F[_]](implicit ev: Bimonad[F]): BimonadLaws[F] =
    new BimonadLaws[F] { def F: Bimonad[F] = ev }
}
