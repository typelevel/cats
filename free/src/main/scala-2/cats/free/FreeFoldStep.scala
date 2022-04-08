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

package cats.free

import Free.{FlatMapped, Pure, Suspend}
import cats.{Eval, Foldable}

private[free] trait FreeFoldStep[S[_], A] {

  def step: Free[S, A]

  /**
   * A combination of step and fold. May be used to define interpreters with custom
   * (non-monoidial) control flow.
   */
  final def foldStep[B](
    onPure: A => B,
    onSuspend: S[A] => B,
    onFlatMapped: ((S[X], X => Free[S, A]) forSome { type X }) => B
  ): B =
    this.step match {
      case Pure(a)                    => onPure(a)
      case Suspend(a)                 => onSuspend(a)
      case FlatMapped(Suspend(fa), f) => onFlatMapped((fa, f))
      case _                          => sys.error("FlatMapped should be right associative after step")
    }

  final def foldLeft[B](fa: Free[S, A], b: B)(f: (B, A) => B)(implicit F: Foldable[S]): B =
    fa.foldStep(
      a => f(b, a),
      fa => F.foldLeft(fa, b)(f),
      { case (fx, g) => F.foldLeft(fx, b)((bb, x) => foldLeft(g(x), bb)(f)) }
    )

  final def foldRight[B](fa: Free[S, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[S]): Eval[B] =
    fa.foldStep(
      a => f(a, lb),
      fa => F.foldRight(fa, lb)(f),
      { case (fx, g) => F.foldRight(fx, lb)((a, lbb) => foldRight(g(a), lbb)(f)) }
    )
}
