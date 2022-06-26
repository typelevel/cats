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

trait UnorderedFoldableSyntax extends UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxUnorderedFoldableOps[F[_]: UnorderedFoldable, A](fa: F[A]): UnorderedFoldableOps[F, A] =
    new UnorderedFoldableOps[F, A](fa)
}

final class UnorderedFoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Tests if this `F[A]` contains `v` using the `Eq` instance for `A`,
   * named contains_ to avoid conflict with existing contains which uses universal equality.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> l.contains_(1)
   * res0: Boolean = true
   * scala> l.contains_(5)
   * res1: Boolean = false
   * }}}
   */
  def contains_(v: A)(implicit ev: Eq[A], F: UnorderedFoldable[F]): Boolean =
    F.contains_(fa, v)

  /**
   * Count the number of elements in the structure that satisfy the given predicate.
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val map1 = Map[Int, String]()
   * scala> val p1: String => Boolean = _.length > 0
   * scala> map1.count(p1)
   * res0: Long = 0
   *
   * scala> val map2 = Map(1 -> "hello", 2 -> "world", 3 -> "!")
   * scala> val p2: String => Boolean = _.length > 1
   * scala> map2.count(p2)
   * res1: Long = 2
   * }}}
   */
  def count(p: A => Boolean)(implicit F: UnorderedFoldable[F]): Long =
    F.count(fa)(p)
}
