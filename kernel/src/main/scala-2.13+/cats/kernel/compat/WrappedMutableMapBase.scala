package cats.kernel
package compat

import scala.collection.mutable

abstract private[kernel] class WrappedMutableMapBase[K, V](m: mutable.Map[K, V]) extends Map[K, V] with Serializable {
  def updated[V2 >: V](key: K, value: V2): Map[K, V2] = m.toMap + ((key, value))
  def removed(key: K): Map[K, V] = m.toMap - key
}
