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

/* This file is derived in part from https://github.com/scala/scala/blob/v2.12.2/src/library/scala/util/hashing/MurmurHash3.scala
 * Modified by Typelevel for redistribution in Cats.
 *
 * Copyright EPFL and Lightbend, Inc.
 * Scala
 * Copyright (c) 2002-2022 EPFL
 * Copyright (c) 2011-2022 Lightbend, Inc.
 *
 * Scala includes software developed at
 * LAMP/EPFL (https://lamp.epfl.ch/) and
 * Lightbend, Inc. (https://www.lightbend.com/).
 *
 * Licensed under the Apache License, Version 2.0 (the "License").
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats
package kernel
package instances

import scala.collection.immutable.{IndexedSeq => ImIndexedSeq}
import scala.collection.mutable
import compat.scalaVersionSpecific.*

@suppressUnusedImportWarningForScalaVersionSpecific
object StaticMethods extends cats.kernel.compat.HashCompat {

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  private[kernel] class WrappedMutableMap[K, V](m: mutable.Map[K, V])
      extends kernel.compat.WrappedMutableMapBase[K, V](m) {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
  }

  /**
   * When you "own" this m, and will not mutate it again, this
   * is safe to call. It is unsafe to call this, then mutate
   * the original collection.
   *
   * You are giving up ownership when calling this method
   */
  def wrapMutableIndexedSeq[A](m: mutable.IndexedSeq[A]): ImIndexedSeq[A] =
    new WrappedIndexedSeq(m)

  private[kernel] class WrappedIndexedSeq[A](m: mutable.IndexedSeq[A]) extends ImIndexedSeq[A] {
    override def length: Int = m.length
    override def apply(i: Int): A = m(i)
    override def iterator: Iterator[A] = m.iterator
  }

  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Order[A]): Int = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next()
          val y = ys.next()
          val cmp = ev.compare(x, y)
          if (cmp != 0) return cmp
        } else {
          return 1
        }
      } else {
        return if (ys.hasNext) -1 else 0
      }
    }
    0
  }

  def iteratorPartialCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: PartialOrder[A]): Double = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next()
          val y = ys.next()
          val cmp = ev.partialCompare(x, y)
          if (cmp != 0.0) return cmp
        } else {
          return 1.0
        }
      } else {
        return if (ys.hasNext) -1.0 else 0.0
      }
    }
    0.0
  }

  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Eq[A]): Boolean = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          if (ev.neqv(xs.next(), ys.next())) return false
        } else {
          return false
        }
      } else {
        return !ys.hasNext
      }
    }
    true
  }

  def combineNIterable[A, R](b: mutable.Builder[A, R], x: Iterable[A], n: Int): R = {
    var i = n
    while (i > 0) { b ++= x; i -= 1 }
    b.result()
  }

  def combineAllIterable[A, R](b: mutable.Builder[A, R], xs: IterableOnce[Iterable[A]]): R = {
    xs.iterator.foreach(b ++= _)
    b.result()
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  def product1Hash(_1Hash: Int): Int = {
    import scala.util.hashing.MurmurHash3.*
    var h = productSeed
    h = mix(h, _1Hash)
    finalizeHash(h, 1)
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  def product2Hash(_1Hash: Int, _2Hash: Int): Int = {
    import scala.util.hashing.MurmurHash3.*
    var h = productSeed
    h = mix(h, _1Hash)
    h = mix(h, _2Hash)
    finalizeHash(h, 2)
  }
}
