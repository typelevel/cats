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

import cats.syntax.apply.*
import cats.syntax.functor.*

/**
 * Laws that must be obeyed by any `Apply`.
 */
trait ApplyLaws[F[_]] extends FunctorLaws[F] with SemigroupalLaws[F] {
  implicit override def F: Apply[F]

  def applyComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    fbc.ap(fab.ap(fa)) <-> fbc.map(compose).ap(fab).ap(fa)
  }

  def map2ProductConsistency[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map(F.product(fa, fb)) { case (a, b) => f(a, b) } <-> F.map2(fa, fb)(f)

  def map2EvalConsistency[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map2(fa, fb)(f) <-> F.map2Eval(fa, Eval.now(fb))(f).value

  def productRConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[B]] =
    F.productR(fa)(fb) <-> F.map2(fa, fb)((_, b) => b)

  def productLConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[A]] =
    F.productL(fa)(fb) <-> F.map2(fa, fb)((a, _) => a)
}

object ApplyLaws {
  def apply[F[_]](implicit ev: Apply[F]): ApplyLaws[F] =
    new ApplyLaws[F] { def F: Apply[F] = ev }
}
