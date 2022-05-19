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

trait EqSyntax {

  /**
   * not final so it can be disabled in favor of scalactic equality in tests
   */
  implicit def catsSyntaxEq[A: Eq](a: A): EqOps[A] =
    new EqOps[A](a)
}

final class EqOps[A: Eq](lhs: A) {
  def ===(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
  def =!=(rhs: A): Boolean = Eq[A].neqv(lhs, rhs)
  def eqv(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
  def neqv(rhs: A): Boolean = Eq[A].neqv(lhs, rhs)
}
