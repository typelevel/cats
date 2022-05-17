package cats.data

import scala.collection.immutable.AbstractMap
import scala.util.hashing.MurmurHash3

private[data] trait HashMapCompatCompanion {
  private[data] class WrappedHashMap[K, V](private[WrappedHashMap] val hashMap: HashMap[K, V]) extends AbstractMap[K, V] {
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
    final override def hashCode: Int = MurmurHash3.mapHash(this)
    final override def equals(that: Any): Boolean = that match {
      case map: WrappedHashMap[_, _] =>
        (this eq map) || (this.hashMap == map.hashMap)
      case other =>
        super.equals(other)
    }
  }
}
