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

import cats.kernel.Comparison

trait OrderSyntax extends PartialOrderSyntax {
  implicit final def catsSyntaxOrder[A: Order](a: A): OrderOps[A] =
    new OrderOps[A](a)
}

final class OrderOps[A: Order](lhs: A) {
  def compare(rhs: A): Int = Order[A].compare(lhs, rhs)
  def min(rhs: A): A = Order[A].min(lhs, rhs)
  def max(rhs: A): A = Order[A].max(lhs, rhs)
  def comparison(rhs: A): Comparison = Order[A].comparison(lhs, rhs)
}
