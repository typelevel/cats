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
package instances

import scala.annotation.nowarn
import scala.collection.mutable

import algebra.ring.*

package object map extends MapInstances

trait MapInstances extends cats.kernel.instances.MapInstances with MapInstances3

trait MapInstances3 extends MapInstances2 {}

trait MapInstances2 extends MapInstances1 {
  implicit def mapSemiring[K, V: Semiring]: MapSemiring[K, V] =
    new MapSemiring[K, V]
}

trait MapInstances1 extends MapInstances0 {}

trait MapInstances0 {
  implicit def mapAdditiveMonoid[K, V: AdditiveSemigroup]: MapAdditiveMonoid[K, V] =
    new MapAdditiveMonoid[K, V]
}

class MapAdditiveMonoid[K, V](implicit V: AdditiveSemigroup[V]) extends AdditiveMonoid[Map[K, V]] {
  def zero: Map[K, V] = Map.empty

  def plus(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) { case (my, (k, x)) =>
        my.updated(k,
                   my.get(k) match {
                     case Some(y) => V.plus(x, y)
                     case None    => x
                   }
        )
      }
    } else {
      ys.foldLeft(xs) { case (mx, (k, y)) =>
        mx.updated(k,
                   mx.get(k) match {
                     case Some(x) => V.plus(x, y)
                     case None    => y
                   }
        )
      }
    }

  override def sumN(a: Map[K, V], n: Int): Map[K, V] =
    if (n > 0) a.map { case (k, v) => (k, V.sumN(v, n)) }
    else if (n == 0) zero
    else throw new IllegalArgumentException("Illegal negative exponent to sumN: %s".format(n))

  @nowarn("msg=deprecated")
  override def sum(as: TraversableOnce[Map[K, V]]): Map[K, V] = {
    val acc = mutable.Map.empty[K, V]
    as.foreach { m =>
      val it = m.iterator
      while (it.hasNext) {
        val (k, y) = it.next()
        acc.get(k) match {
          case None    => acc(k) = y
          case Some(x) => acc(k) = V.plus(x, y)
        }
      }
    }
    cats.kernel.instances.StaticMethods.wrapMutableMap(acc)
  }
}

class MapSemiring[K, V](implicit V: Semiring[V]) extends MapAdditiveMonoid[K, V] with Semiring[Map[K, V]] {

  def times(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    // we figure out which of our maps is smaller, iterate over its
    // keys, see which of those are in the larger map, and add the
    // resulting product to our result map.
    if (xs.size <= ys.size) {
      xs.foldLeft(Map.empty[K, V]) { case (m, (k, x)) =>
        ys.get(k) match {
          case Some(y) => m.updated(k, V.times(x, y))
          case None    => m
        }
      }
    } else {
      ys.foldLeft(Map.empty[K, V]) { case (m, (k, y)) =>
        xs.get(k) match {
          case Some(x) => m.updated(k, V.times(x, y))
          case None    => m
        }
      }
    }

  override def pow(x: Map[K, V], n: Int): Map[K, V] =
    if (n < 1) throw new IllegalArgumentException(s"non-positive exponent: $n")
    else if (n == 1) x
    else x.map { case (k, v) => (k, V.pow(v, n)) }

  @nowarn("msg=deprecated")
  override def tryProduct(as: TraversableOnce[Map[K, V]]): Option[Map[K, V]] =
    if (as.isEmpty) {
      None
    } else {
      val acc = mutable.Map.empty[K, V]
      var ready: Boolean = false
      as.foreach { m =>
        if (ready) {
          // at this point all we can do is modify or remove
          // keys. since any "missing" key is effectively zero, and
          // since 0 * x = 0, we ignore any keys not already in our
          // accumulator.
          val it = acc.iterator
          while (it.hasNext) {
            val (k, x) = it.next()
            m.get(k) match {
              case None    => acc -= k
              case Some(y) => acc(k) = V.times(x, y)
            }
          }
        } else {
          // we have to initialize our accumulator (`acc`) with the
          // very first element of `as`. if there is only one map in
          // our collection we want to return exactly those values.
          val it = m.iterator
          while (it.hasNext) acc += it.next()
          ready = true
        }
      }
      Some(cats.kernel.instances.StaticMethods.wrapMutableMap(acc))
    }
}
