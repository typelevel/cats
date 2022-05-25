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
package data

/** A `Function1` with constant value and strictly evaluated combinators. Not suitable
 * for composing with side-effecting functions. */
final private[data] case class StrictConstFunction1[A](a: A) extends Function1[Any, A] {
  def apply(arg: Any): A = a

  /** Creates a new `StrictConstFunction1` by applying `g` to this function's constant value.
   * `g` will not be evaluated when the resulting function is subsequently run. Not stack-safe. */
  override def andThen[B](g: A => B): Any => B = g match {
    case g: StrictConstFunction1[B] => g
    case _                          => StrictConstFunction1(g(a))
  }

  /** This is a no-op; `g` will never be used. */
  override def compose[A0](g: A0 => Any): A0 => A = this
}
