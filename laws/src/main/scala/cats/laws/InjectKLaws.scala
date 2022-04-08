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
package laws

trait InjectKLaws[F[_], G[_]] {
  def injectK: InjectK[F, G]

  def injectKRoundTripInj[A](fa: F[A]): IsEq[Option[F[A]]] =
    injectK.prj.compose(injectK.inj).apply(fa) <-> Some(fa)

  def injectKRoundTripPrj[A](ga: G[A]): IsEq[Option[G[A]]] =
    injectK.prj(ga) match {
      case Some(fa) => (Some(injectK.inj(fa)): Option[G[A]]) <-> Some(ga)
      case None     => (None: Option[G[A]]) <-> None
    }
}

object InjectKLaws {
  def apply[F[_], G[_]](implicit ev: InjectK[F, G]): InjectKLaws[F, G] =
    new InjectKLaws[F, G] { val injectK: InjectK[F, G] = ev }
}
