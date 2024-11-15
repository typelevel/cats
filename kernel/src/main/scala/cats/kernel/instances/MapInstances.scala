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

import org.typelevel.scalaccompat.annotation.unused

import scala.collection.mutable

import compat.scalaVersionSpecific.*

@suppressUnusedImportWarningForScalaVersionSpecific
trait MapInstances extends MapInstances1 {
  implicit def catsKernelStdHashForMap[K, V](implicit @unused K: Hash[K], V: Hash[V]): Hash[Map[K, V]] =
    new MapHash[K, V]

  implicit def catsKernelStdCommutativeMonoidForMap[K, V: CommutativeSemigroup]: CommutativeMonoid[Map[K, V]] =
    new MapMonoid[K, V] with CommutativeMonoid[Map[K, V]]
}

private[instances] trait MapInstances1 {
  implicit def catsKernelStdEqForMap[K, V: Eq]: Eq[Map[K, V]] =
    new MapEq[K, V]
  implicit def catsKernelStdMonoidForMap[K, V: Semigroup]: Monoid[Map[K, V]] =
    new MapMonoid[K, V]
}

class MapHash[K, V](implicit V: Hash[V]) extends MapEq[K, V]()(V) with Hash[Map[K, V]] {
  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3.*
  def hash(x: Map[K, V]): Int = {
    var a, b, n = 0
    var c = 1
    x.foreach { case (k, v) =>
      // use the default hash on keys because that's what Scala's Map does
      val h = StaticMethods.product2HashWithPrefix(k.hashCode(), V.hash(v), "Tuple2")
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

class MapEq[K, V](implicit V: Eq[V]) extends Eq[Map[K, V]] {
  def eqv(x: Map[K, V], y: Map[K, V]): Boolean =
    if (x eq y) true
    else
      x.size == y.size && x.forall { case (k, v1) =>
        y.get(k) match {
          case Some(v2) => V.eqv(v1, v2)
          case None     => false
        }
      }
}

class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]] {

  def empty: Map[K, V] = Map.empty

  def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) { case (my, (k, x)) =>
        my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) { case (mx, (k, y)) =>
        mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }

  override def combineAll(xss: IterableOnce[Map[K, V]]): Map[K, V] = {
    val acc = mutable.Map.empty[K, V]
    xss.iterator.foreach { m =>
      val it = m.iterator
      while (it.hasNext) {
        val (k, v) = it.next()
        acc(k) = Semigroup.maybeCombine(acc.get(k), v)
      }
    }
    StaticMethods.wrapMutableMap(acc)
  }
}
