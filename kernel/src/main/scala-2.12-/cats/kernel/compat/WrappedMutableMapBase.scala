package cats.kernel
package compat

import scala.collection.mutable


private[kernel] abstract class WrappedMutableMapBase[K, V](m: mutable.Map[K, V]) extends Map[K, V] {
  def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
  def -(key: K): Map[K, V] = m.toMap - key
}
