package cats.kernel
package compat

import scala.collection.mutable

abstract private[kernel] class WrappedMutableMapBase[K, V](m: mutable.Map[K, V]) extends Map[K, V] with Serializable {
  def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
  def -(key: K): Map[K, V] = m.toMap - key
}
