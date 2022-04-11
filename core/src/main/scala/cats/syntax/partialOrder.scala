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

trait PartialOrderSyntax extends EqSyntax {
  implicit final def catsSyntaxPartialOrder[A: PartialOrder](a: A): PartialOrderOps[A] =
    new PartialOrderOps[A](a)
}

final class PartialOrderOps[A](lhs: A)(implicit A: PartialOrder[A]) {
  def >(rhs: A): Boolean = A.gt(lhs, rhs)
  def >=(rhs: A): Boolean = A.gteqv(lhs, rhs)
  def <(rhs: A): Boolean = A.lt(lhs, rhs)
  def <=(rhs: A): Boolean = A.lteqv(lhs, rhs)

  def partialCompare(rhs: A): Double = A.partialCompare(lhs, rhs)
  def partialComparison(rhs: A): Option[Comparison] = A.partialComparison(lhs, rhs)
  def tryCompare(rhs: A): Option[Int] = A.tryCompare(lhs, rhs)
  def pmin(rhs: A): Option[A] = A.pmin(lhs, rhs)
  def pmax(rhs: A): Option[A] = A.pmax(lhs, rhs)
}
