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

import scala.collection.immutable.AbstractSet

private[data] trait HashSetCompatCompanion {
  private[data] class WrappedHashSet[A](private[WrappedHashSet] val hashSet: HashSet[A]) extends AbstractSet[A] {
    def iterator: Iterator[A] = hashSet.iterator
    def incl(elem: A): Set[A] = new WrappedHashSet(hashSet.add(elem))
    def excl(elem: A): Set[A] = new WrappedHashSet(hashSet.remove(elem))
    def contains(elem: A): Boolean = hashSet.contains(elem)
    override def size: Int = hashSet.size
    override def knownSize: Int = hashSet.size
    override def isEmpty: Boolean = hashSet.isEmpty
    override def foreach[U](f: A => U): Unit = hashSet.foreach(f)
    override def concat(that: IterableOnce[A]): Set[A] = new WrappedHashSet(hashSet.union(that))
    override def diff(that: scala.collection.Set[A]): Set[A] = new WrappedHashSet(hashSet.diff(that))
    override def intersect(that: scala.collection.Set[A]): Set[A] = new WrappedHashSet(hashSet.intersect(that))
    override def filter(pred: A => Boolean): Set[A] = new WrappedHashSet(hashSet.filter(pred))
    override def filterNot(pred: A => Boolean): Set[A] = new WrappedHashSet(hashSet.filterNot(pred))
    override def hashCode: Int = hashSet.hashCode
    override def equals(that: Any): Boolean = that match {
      case set: WrappedHashSet[_] =>
        this.hashSet == set.hashSet
      case other =>
        super.equals(other)
    }
  }
}
