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

package cats.tests.compat

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Seq
import scala.collection.mutable

final private[tests] class SeqOps[C[a] <: Seq[a], A] private[compat] (private val self: C[A]) extends AnyVal {

  // Scala v2.12.x does not have `distinctBy` implemented.
  // Therefore this implementation is copied (and adapted) from Scala Library v2.13.8 sources:
  // https://github.com/scala/scala/blob/v2.13.8/src/library/scala/collection/immutable/StrictOptimizedSeqOps.scala#L26-L39
  def distinctBy[B](f: A => B)(implicit cbf: CanBuildFrom[C[A], A, C[A]]): C[A] = {
    if (self.lengthCompare(1) <= 0) self
    else {
      val builder = cbf()
      val seen = mutable.HashSet.empty[B]
      val it = self.iterator
      var different = false
      while (it.hasNext) {
        val next = it.next()
        if (seen.add(f(next))) builder += next else different = true
      }
      if (different) builder.result() else self
    }
  }
}
