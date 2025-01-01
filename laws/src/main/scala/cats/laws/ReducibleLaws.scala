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

import cats.syntax.all.*

trait ReducibleLaws[F[_]] extends FoldableLaws[F] {
  implicit def F: Reducible[F]

  def reduceLeftToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceLeftTo(f)((b, a) => b |+| f(a))

  def reduceRightToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).value

  def reduceRightToConsistentWithReduceRightToOption[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[Option[B]] =
    fa.reduceRightToOption(f)((a, eb) => eb.map(f(a) |+| _)).value <->
      fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).map(Option(_)).value

  def reduceRightConsistentWithReduceRightOption[A](fa: F[A], f: (A, A) => A): IsEq[Option[A]] =
    fa.reduceRight((a1, e2) => e2.map(f(a1, _))).map(Option(_)).value <->
      fa.reduceRightOption((a1, e2) => e2.map(f(a1, _))).value

  def reduceReduceLeftConsistent[B](fa: F[B])(implicit B: Semigroup[B]): IsEq[B] =
    fa.reduce <-> fa.reduceLeft(B.combine)

  def traverseConsistent[G[_]: Applicative, A, B](fa: F[A], f: A => G[B]): IsEq[G[Unit]] =
    fa.nonEmptyTraverseVoid(f) <-> fa.traverseVoid(f)

  def sequenceConsistent[G[_]: Applicative, A](fa: F[G[A]]): IsEq[G[Unit]] =
    fa.nonEmptySequenceVoid <-> fa.sequenceVoid

  def sizeConsistent[A](fa: F[A]): IsEq[Long] =
    fa.size <-> fa.reduceMap(_ => 1L)
}

object ReducibleLaws {
  def apply[F[_]](implicit ev: Reducible[F]): ReducibleLaws[F] =
    new ReducibleLaws[F] { def F: Reducible[F] = ev }
}
