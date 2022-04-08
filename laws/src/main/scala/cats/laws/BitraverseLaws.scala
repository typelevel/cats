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

import cats.data.Nested

trait BitraverseLaws[F[_, _]] extends BifoldableLaws[F] with BifunctorLaws[F] {
  implicit override def F: Bitraverse[F]

  def bitraverseIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> F.bitraverse[Id, A, B, A, B](fab)(identity, identity)

  def bitraverseCompose[G[_], A, B, C, D, E, H](
    fab: F[A, B],
    f: A => G[C],
    g: B => G[D],
    h: C => G[E],
    i: D => G[H]
  )(implicit G: Applicative[G]): IsEq[G[G[F[E, H]]]] = {
    val fg = F.bitraverse(fab)(f, g)
    val hi = G.map(fg)(f => F.bitraverse(f)(h, i))

    val c =
      F.bitraverse[Nested[G, G, *], A, B, E, H](fab)(
        a => Nested(G.map(f(a))(h)),
        b => Nested(G.map(g(b))(i))
      )

    hi <-> c.value
  }
}

object BitraverseLaws {
  def apply[F[_, _]](implicit ev: Bitraverse[F]): BitraverseLaws[F] =
    new BitraverseLaws[F] { def F: Bitraverse[F] = ev }
}
