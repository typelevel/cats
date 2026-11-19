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
package syntax

import cats.kernel._

trait PartialNextSyntax {
  implicit final def catsSyntaxPartialNext[A](a: A): PartialNextOps[A] =
    new PartialNextOps(a)
}

final class PartialNextOps[A](private val lhs: A) extends AnyVal {
  def partialNext(implicit A: PartialNext[A]): Option[A] =
    A.partialNext(lhs)
}

trait NextSyntax extends PartialNextSyntax {
  implicit final def catsSyntaxNext[A](a: A): NextOps[A] =
    new NextOps(a)
}

final class NextOps[A](private val lhs: A) extends AnyVal {
  def next(implicit A: Next[A]): A =
    A.next(lhs)
}

trait PartialPreviousSyntax {
  implicit final def catsSyntaxPartialPrevious[A](a: A): PartialPreviousOps[A] =
    new PartialPreviousOps(a)
}

final class PartialPreviousOps[A](private val lhs: A) extends AnyVal {
  def partialPrevious(implicit A: PartialPrevious[A]): Option[A] =
    A.partialPrevious(lhs)
}

trait PreviousSyntax extends PartialPreviousSyntax {
  implicit final def catsSyntaxPrevious[A](a: A): PreviousOps[A] =
    new PreviousOps(a)
}

final class PreviousOps[A](private val lhs: A) extends AnyVal {
  def previous(implicit A: Previous[A]): A =
    A.previous(lhs)
}

trait BoundedEnumerableSyntax extends NextSyntax with PreviousSyntax {
  implicit final def catsSyntaxForBoundedEnumerable[A](a: A): BoundedEnumerableOps[A] =
    new BoundedEnumerableOps(a)
}

final class BoundedEnumerableOps[A](private val lhs: A) extends AnyVal {
  def cycleNext(implicit A0: PartialNext[A], A1: LowerBounded[A]): A =
    A0.partialNext(lhs).getOrElse(A1.minBound)

  def cyclePrevious(implicit A0: PartialPrevious[A], A1: UpperBounded[A]): A =
    A0.partialPrevious(lhs).getOrElse(A1.maxBound)
}
