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

trait BifoldableLaws[F[_, _]] {
  implicit def F: Bifoldable[F]

  def bifoldLeftConsistentWithBifoldMap[A, B, C](fab: F[A, B], f: A => C, g: B => C)(implicit C: Monoid[C]): IsEq[C] = {
    val expected = F.bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )
    expected <-> F.bifoldMap(fab)(f, g)
  }

  def bifoldRightConsistentWithBifoldMap[A, B, C](fab: F[A, B], f: A => C, g: B => C)(implicit
    C: Monoid[C]
  ): IsEq[C] = {
    val expected = F.bifoldRight(fab, Later(C.empty))(
      (a: A, ec: Eval[C]) => ec.map(c => C.combine(f(a), c)),
      (b: B, ec: Eval[C]) => ec.map(c => C.combine(g(b), c))
    )
    expected.value <-> F.bifoldMap(fab)(f, g)
  }
}

object BifoldableLaws {
  def apply[F[_, _]](implicit ev: Bifoldable[F]): BifoldableLaws[F] =
    new BifoldableLaws[F] {
      def F: Bifoldable[F] = ev
    }
}
