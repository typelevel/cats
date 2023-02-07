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

import cats.data.Nested

trait NestedSyntax {
  implicit final def catsSyntaxNestedId[F[_], G[_], A](value: F[G[A]]): NestedIdOps[F, G, A] =
    new NestedIdOps[F, G, A](value)
}

final class NestedIdOps[F[_], G[_], A](private val value: F[G[A]]) extends AnyVal {

  /**
   * Wrap a value in `Nested`.
   *
   * `x.nested` is equivalent to `Nested(x)`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> List(Some(3), None).nested.map(_+1).value
   * res0: List[Option[Int]] = List(Some(4), None)
   * }}}
   */
  def nested: Nested[F, G, A] = Nested[F, G, A](value)
}
