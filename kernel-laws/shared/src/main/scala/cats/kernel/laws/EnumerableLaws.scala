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

package cats.kernel
package laws

trait PartialPreviousLaws[A] extends PartialOrderLaws[A] {

  implicit def P: PartialPrevious[A]

  def previousOrderWeak(a: A): IsEq[Boolean] =
    P.partialPrevious(a).map(E.lt(_, a)).getOrElse(true) <-> true

  def previousOrderStrong(a: A, b: A): IsEq[Option[Boolean]] =
    if (E.gt(a, b)) {
      P.partialPrevious(a).map(E.gteqv(_, b)) <-> Option(true)
    } else {
      Option.empty <-> Option.empty
    }

}

object PartialPreviousLaws {
  def apply[A](implicit ev: PartialPrevious[A]): PartialPreviousLaws[A] =
    new PartialPreviousLaws[A] {
      def E: PartialOrder[A] = ev.partialOrder
      def P: PartialPrevious[A] = ev
    }
}

trait PartialNextLaws[A] extends PartialOrderLaws[A] {

  implicit def N: PartialNext[A]

  def nextOrderWeak(a: A): IsEq[Boolean] =
    N.partialNext(a).map(E.gt(_, a)).getOrElse(true) <-> true

  def nextOrderStrong(a: A, b: A): IsEq[Option[Boolean]] =
    if (E.lt(a, b)) {
      N.partialNext(a).map(E.lteqv(_, b)) <-> Option(true)
    } else {
      Option(true) <-> Option(true)
    }

}

trait PartialNextBoundedLaws[A] extends PartialNextLaws[A] with UpperBoundedLaws[A] {

  def minBoundTerminal: IsEq[Option[A]] =
    N.partialNext(B.maxBound) <-> None

}

trait PartialPreviousNextLaws[A] extends PartialNextLaws[A] with PartialPreviousLaws[A] with OrderLaws[A] {

  def partialLeftIdentity(a: A): IsEq[Option[A]] =
    P.partialPrevious(a)
      .map(N.partialNext(_) <-> Some(a))
      .getOrElse(Option.empty <-> Option.empty)

  def partialRightIdentity(a: A): IsEq[Option[A]] =
    N.partialNext(a)
      .map(P.partialPrevious(_) <-> Some(a))
      .getOrElse(Option.empty <-> Option.empty)

}

trait PartialPreviousBoundedLaws[A] extends PartialPreviousLaws[A] with LowerBoundedLaws[A] {

  def maxBoundTerminal: IsEq[Option[A]] =
    P.partialPrevious(B.minBound) <-> None

}

trait BoundedEnumerableLaws[A]
    extends PartialPreviousNextLaws[A]
    with PartialPreviousBoundedLaws[A]
    with PartialNextBoundedLaws[A] {
  override def B: LowerBounded[A] with UpperBounded[A]
}

object BoundedEnumerableLaws {
  def apply[A](implicit ev: BoundedEnumerable[A]): BoundedEnumerableLaws[A] =
    new BoundedEnumerableLaws[A] {
      val B: LowerBounded[A] with UpperBounded[A] = ev
      val E = ev.order
      val N: PartialNext[A] = ev
      val P: PartialPrevious[A] = ev
    }
}
