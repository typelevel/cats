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

package cats.kernel.laws

import cats.kernel.{LowerBounded, PartialOrder, UpperBounded}

trait LowerBoundedLaws[A] extends PartialOrderLaws[A] {
  implicit def B: LowerBounded[A]

  def boundLteqv(x: A): IsEq[Boolean] =
    E.lteqv(B.minBound, x) <-> true
}

object LowerBoundedLaws {
  def apply[A](implicit ev: LowerBounded[A]): LowerBoundedLaws[A] =
    new LowerBoundedLaws[A] {
      def B: LowerBounded[A] = ev
      def E: PartialOrder[A] = ev.partialOrder
    }
}

trait UpperBoundedLaws[A] extends PartialOrderLaws[A] {
  implicit def B: UpperBounded[A]

  def boundGteqv(x: A): IsEq[Boolean] =
    E.gteqv(B.maxBound, x) <-> true
}

object UpperBoundedLaws {
  def apply[A](implicit ev: UpperBounded[A]): UpperBoundedLaws[A] =
    new UpperBoundedLaws[A] {
      def B: UpperBounded[A] = ev
      def E: PartialOrder[A] = ev.partialOrder
    }
}
