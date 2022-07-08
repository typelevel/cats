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
package kernel

import scala.{specialized => sp}
import scala.collection.immutable.Stream

trait PartialPreviousUpperBounded[@sp A] extends PartialPrevious[A] with PartialNext[A] with UpperBounded[A] {

  /**
   * Enumerate the members in descending order.
   */
  def membersDescending: Stream[A] = {
    def loop(a: A): Stream[A] =
      partialPrevious(a) match {
        case Some(aa) => aa #:: loop(aa)
        case _        => Stream.empty
      }
    maxBound #:: loop(maxBound)
  }

}

trait PartialNextLowerBounded[@sp A] extends PartialPrevious[A] with PartialNext[A] with LowerBounded[A] {

  /**
   * Enumerate the members in ascending order.
   */
  def membersAscending: Stream[A] = {
    def loop(a: A): Stream[A] =
      partialNext(a) match {
        case Some(aa) => aa #:: loop(aa)
        case _        => Stream.empty
      }
    minBound #:: loop(minBound)
  }

}
