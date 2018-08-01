package cats.kernel
package compat

import scala.collection.mutable


private[kernel] abstract class WrappedMutableMapBase[K, V](m: mutable.Map[K, V]) extends Map[K, V] {
  def updated[V2 >: V](key: K, value: V2): Map[K, V2] = m.toMap + ((key, value))
  def remove(key: K): Map[K, V] = m.toMap - key
}
