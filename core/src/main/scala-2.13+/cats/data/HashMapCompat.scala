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

private[data] trait HashMapCompat[K, +V] extends IterableOnce[(K, V)] { self: HashMap[K, V] =>

  import HashMap.improve

  override def knownSize = self.size

  /**
    * Creates a new map with all key-value pairs of this map, and all key-value pairs of `iterable`.
    *
    * @param iterable the collection of key-value pairs to be added.
    * @return a new map that contains all key-value pairs of this map and `iterable`.
    */
  final def concat[VV >: V](iterable: IterableOnce[(K, VV)]): HashMap[K, VV] = {
    val newRootNode = iterable match {
      case hm: HashMap[K, V] @unchecked if self.size <= hm.size =>
        self.iterator.foldLeft(hm.rootNode: HashMap.Node[K, VV]) { case (node, (k, v)) =>
          node.updated(k, improve(self.hashKey.hash(k)), v, replaceExisting = false, 0)
        }
      case _ =>
        iterable.iterator.foldLeft(self.rootNode: HashMap.Node[K, VV]) { case (node, (k, v)) =>
          node.updated(k, improve(self.hashKey.hash(k)), v, replaceExisting = true, 0)
        }
    }

    if (newRootNode eq self.rootNode)
      this
    else
      new HashMap(newRootNode)
  }
}
