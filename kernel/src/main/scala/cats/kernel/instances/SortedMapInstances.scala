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

import scala.collection.immutable.SortedMap

trait SortedMapInstances extends SortedMapInstances3 {
  implicit def catsKernelStdHashForSortedMap[K: Hash, V: Hash]: Hash[SortedMap[K, V]] =
    new SortedMapHash[K, V]

  @deprecated("Use catsKernelStdHashForSortedMap override without Order", "2.2.0-M3")
  def catsKernelStdHashForSortedMap[K, V](hashK: Hash[K], orderK: Order[K], hashV: Hash[V]): Hash[SortedMap[K, V]] =
    new SortedMapHash[K, V]()(hashV, hashK)

  implicit def catsKernelStdCommutativeSemigroupForSortedMap[K, V: CommutativeSemigroup]
    : CommutativeSemigroup[SortedMap[K, V]] =
    new SortedMapCommutativeSemigroup[K, V]

  implicit def catsKernelStdCommutativeMonoidForSortedMap[K: Order, V: CommutativeSemigroup]
    : CommutativeMonoid[SortedMap[K, V]] =
    new SortedMapCommutativeMonoid[K, V]
}

private[instances] trait SortedMapInstances1 {
  implicit def catsKernelStdEqForSortedMap[K, V: Eq]: Eq[SortedMap[K, V]] =
    new SortedMapEq[K, V]

  @deprecated("Use catsKernelStdEqForSortedMap override without Order", "2.2.0-M3")
  def catsKernelStdEqForSortedMap[K, V](orderK: Order[K], eqV: Eq[V]): Eq[SortedMap[K, V]] =
    new SortedMapEq[K, V]()(eqV)
}

private[instances] trait SortedMapInstances2 extends SortedMapInstances1 {
  implicit def catsKernelStdSemigroupForSortedMap[K, V: Semigroup]: Semigroup[SortedMap[K, V]] =
    new SortedMapSemigroup[K, V]

  implicit def catsKernelStdMonoidForSortedMap[K: Order, V: Semigroup]: Monoid[SortedMap[K, V]] =
    new SortedMapMonoid[K, V]

  implicit def catsKernelStdPartialOrderForSortedMap[K, V: PartialOrder]: PartialOrder[SortedMap[K, V]] =
    new SortedMapPartialOrder[K, V]
}

private[instances] trait SortedMapInstances3 extends SortedMapInstances2 {
  implicit def catsKernelStdOrderForSortedMap[K, V: Order]: Order[SortedMap[K, V]] =
    new SortedMapOrder[K, V]
}

private[instances] class SortedMapOrder[K, V](implicit V: Order[V]) extends Order[SortedMap[K, V]] {
  override def compare(x: SortedMap[K, V], y: SortedMap[K, V]): Int = {
    implicit val order: Order[K] = Order.fromOrdering(x.ordering)
    if (x eq y) {
      0
    } else {
      StaticMethods.iteratorCompare(x.iterator, y.iterator)
    }
  }
}

private[instances] class SortedMapPartialOrder[K, V](implicit V: PartialOrder[V])
    extends PartialOrder[SortedMap[K, V]] {
  override def partialCompare(x: SortedMap[K, V], y: SortedMap[K, V]): Double = {
    implicit val order: Order[K] = Order.fromOrdering(x.ordering)

    if (x eq y) {
      0.0
    } else {
      StaticMethods.iteratorPartialCompare(x.iterator, y.iterator)
    }
  }
}

class SortedMapHash[K, V](implicit V: Hash[V], K: Hash[K]) extends SortedMapEq[K, V]()(V) with Hash[SortedMap[K, V]] {

  @deprecated("Use the constructor _without_ Order instead, since Order is not required", "2.2.0-M3")
  private[instances] def this(V: Hash[V], O: Order[K], K: Hash[K]) = this()(V, K)

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3.*
  def hash(x: SortedMap[K, V]): Int = {
    var a, b, n = 0
    var c = 1
    x.foreach { case (k, v) =>
      val h = StaticMethods.product2HashWithPrefix(K.hash(k), V.hash(v), "Tuple2")
      a += h
      b ^= h
      c = StaticMethods.updateUnorderedHashC(c, h)
      n += 1
    }
    var h = mapSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
}

class SortedMapEq[K, V](implicit V: Eq[V]) extends Eq[SortedMap[K, V]] {

  @deprecated("Use the constructor _without_ Order instead, since Order is not required", "2.2.0")
  private[instances] def this(V: Eq[V], O: Order[K]) = this()(V)

  def eqv(x: SortedMap[K, V], y: SortedMap[K, V]): Boolean =
    if (x eq y) true
    else
      x.size == y.size && x.forall { case (k, v1) =>
        y.get(k) match {
          case Some(v2) => V.eqv(v1, v2)
          case None     => false
        }
      }
}

class SortedMapCommutativeSemigroup[K, V](implicit V: CommutativeSemigroup[V])
    extends SortedMapSemigroup[K, V]
    with CommutativeSemigroup[SortedMap[K, V]]

class SortedMapCommutativeMonoid[K, V](implicit V: CommutativeSemigroup[V], O: Order[K])
    extends SortedMapMonoid[K, V]
    with CommutativeMonoid[SortedMap[K, V]]

class SortedMapSemigroup[K, V](implicit V: Semigroup[V]) extends Semigroup[SortedMap[K, V]] {

  def combine(xs: SortedMap[K, V], ys: SortedMap[K, V]): SortedMap[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) { case (my, (k, x)) =>
        my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) { case (mx, (k, y)) =>
        mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }
}

class SortedMapMonoid[K, V](implicit V: Semigroup[V], O: Order[K])
    extends SortedMapSemigroup[K, V]
    with Monoid[SortedMap[K, V]] {

  def empty: SortedMap[K, V] = SortedMap.empty(O.toOrdering)
}
