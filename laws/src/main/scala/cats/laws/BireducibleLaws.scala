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

import cats.data.Ior
import cats.kernel.laws.IsEq

trait BireducibleLaws[F[_, _]] extends BifoldableLaws[F] {
  implicit def F: Bireducible[F]

  def bireduceLeftConsistentWithDefaultImplementation[A, B](
    fab: F[A, B],
    ma: (A, A) => A,
    mb: (B, B) => B
  ): IsEq[A Ior B] = {
    val obtained = F.bireduceLeft(fab)(ma, mb)
    val expected = Bireducible.bireduceLeft(fab)(ma, mb)

    obtained <-> expected
  }

  def bireduceRightConsistentWithDefaultImplementation[A, B](
    fab: F[A, B],
    ma: (A, Eval[A]) => Eval[A],
    mb: (B, Eval[B]) => Eval[B]
  ): IsEq[A Ior B] = {
    val obtained = F.bireduceRight(fab)(ma, mb).value
    val expected = Bireducible.bireduceRight(fab)(ma, mb).value

    obtained <-> expected
  }

  def bireduceMapConsistentWithDefaultImplementation[A, B, C](
    fab: F[A, B],
    ma: A => C,
    mb: B => C
  )(implicit
    C: Semigroup[C]
  ): IsEq[C] = {
    val obtained = F.bireduceMap(fab)(ma, mb)
    val expected = Bireducible.bireduceMap(fab)(ma, mb)

    obtained <-> expected
  }

  def bireduceConsistentWithDefaultImplementation[A, B](
    fab: F[A, B]
  )(implicit A: Semigroup[A], B: Semigroup[B]): IsEq[A Ior B] = {

    val obtained = F.bireduce(fab)
    val expected = Bireducible.bireduce(fab)

    obtained <-> expected
  }

  def bireduceLeftToConsistentWithBireduceRightTo[A, B, C](
    fab: F[A, B],
    ma: A => C,
    mb: B => C,
    mca: (C, A) => C,
    mcb: (C, B) => C
  ): IsEq[C] = {
    val left = F.bireduceLeftTo(fab)(ma, mb)(mca, mcb)
    val right =
      F.bireduceRightTo(fab)(
        a => Eval.now(ma(a)),
        b => Eval.now(mb(b))
      )(
        (a, c) => c.map(mca(_, a)),
        (b, c) => c.map(mcb(_, b))
      ).value

    left <-> right
  }
}

object BireducibleLaws {
  def apply[F[_, _]](implicit ev: Bireducible[F]): BireducibleLaws[F] =
    new BireducibleLaws[F] {
      def F: Bireducible[F] = ev
    }
}
