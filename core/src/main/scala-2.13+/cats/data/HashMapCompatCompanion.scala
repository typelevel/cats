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

package cats.data

import scala.collection.immutable.AbstractMap
import scala.util.hashing.MurmurHash3

private[data] trait HashMapCompatCompanion {
  private[data] class WrappedHashMap[K, V](private[WrappedHashMap] val hashMap: HashMap[K, V])
      extends AbstractMap[K, V] {
    final def iterator: collection.Iterator[(K, V)] = hashMap.iterator
    final def get(key: K): Option[V] = hashMap.get(key)
    final def updated[V1 >: V](key: K, value: V1): Map[K, V1] = new WrappedHashMap(hashMap.updated(key, value))
    final def removed(key: K): Map[K, V] = new WrappedHashMap(hashMap.removed(key))
    final override def size: Int = hashMap.size
    final override def knownSize: Int = hashMap.size
    final override def contains(key: K): Boolean = hashMap.contains(key)
    final override def foreach[U](f: ((K, V)) => U): Unit = hashMap.foreach(Function.untupled(f))
    final override def getOrElse[V1 >: V](key: K, default: => V1): V1 = hashMap.getOrElse(key, default)
    final override def keysIterator: Iterator[K] = hashMap.keysIterator
    final override def valuesIterator: Iterator[V] = hashMap.valuesIterator
    final override def isEmpty: Boolean = hashMap.isEmpty
    final override def nonEmpty: Boolean = hashMap.nonEmpty
    final override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]): Map[K, V2] =
      new WrappedHashMap(hashMap.concat(suffix))
    final override def hashCode: Int = hashMap.hashCode
    final override def equals(that: Any): Boolean = that match {
      case map: WrappedHashMap[_, _] =>
        this.hashMap == map.hashMap
      case other =>
        super.equals(other)
    }
  }
}
