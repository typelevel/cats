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

package algebra

object Instances {

  val stringHasMonoid =
    new Monoid[String] {
      def empty: String = ""
      def combine(x: String, y: String): String = x + y
    }

  def f1ComposeMonoid[A] =
    new Monoid[A => A] {
      def empty: A => A =
        a => a
      def combine(x: A => A, y: A => A): A => A =
        a => y(x(a))
    }

  def f1HomomorphismMonoid[A, B](implicit ev: Monoid[B]) =
    new Monoid[A => B] {
      def empty: A => B =
        _ => ev.empty
      def combine(x: A => B, y: A => B): A => B =
        a => ev.combine(x(a), y(a))
    }
}
