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

package cats.kernel
package instances

import scala.collection.immutable.SortedSet

trait SortedSetInstances extends SortedSetInstances1 {
  @deprecated("Use catsKernelStdHashForSortedSet override without Order", "2.1.0")
  def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    new SortedSetHash[A]

  implicit def catsKernelStdHashForSortedSet[A: Hash]: Hash[SortedSet[A]] =
    new SortedSetHash[A]
}

private[instances] trait SortedSetInstances1 {
  implicit def catsKernelStdOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    new SortedSetOrder[A]

  implicit def catsKernelStdBoundedSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    new SortedSetSemilattice[A]
}

class SortedSetOrder[A: Order] extends Order[SortedSet[A]] {
  def compare(a1: SortedSet[A], a2: SortedSet[A]): Int =
    cats.kernel.instances.int.catsKernelStdOrderForInt.compare(a1.size, a2.size) match {
      case 0 => StaticMethods.iteratorCompare(a1.iterator, a2.iterator)
      case x => x
    }

  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean =
    StaticMethods.iteratorEq(s1.iterator, s2.iterator)
}

// FIXME use context bound in 3.x
class SortedSetHash[A](implicit hashA: Hash[A]) extends Hash[SortedSet[A]] {
  import scala.util.hashing.MurmurHash3.*

  @deprecated("Use the constructor _without_ Order instead, since Order is not required", "2.1.0")
  private[instances] def this(o: Order[A], h: Hash[A]) = this()(h)

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  def hash(xs: SortedSet[A]): Int = {
    var a, b, n = 0
    var c = 1
    xs.foreach { x =>
      val h = Hash[A].hash(x)
      a += h
      b ^= h
      c = cats.kernel.instances.StaticMethods.updateUnorderedHashC(c, h)
      n += 1
    }
    var h = setSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean =
    StaticMethods.iteratorEq(s1.iterator, s2.iterator)(Eq[A])
}

class SortedSetSemilattice[A: Order] extends BoundedSemilattice[SortedSet[A]] {
  def empty: SortedSet[A] = SortedSet.empty(implicitly[Order[A]].toOrdering)
  def combine(x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y
}
