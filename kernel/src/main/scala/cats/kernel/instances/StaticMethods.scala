package cats.kernel
package instances

import scala.collection.mutable

object StaticMethods {

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  private[kernel] class WrappedMutableMap[K, V](m: mutable.Map[K, V]) extends Map[K, V] {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
    def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
    def -(key: K): Map[K, V] = m.toMap - key
  }

  // scalastyle:off return
  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Order[A]): Int = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next
          val y = ys.next
          val cmp = ev.compare(x, y)
          if (cmp != 0) return cmp
        } else {
          return 1
        }
      } else {
        return if (ys.hasNext) -1 else 0
      }
    }
    0
  }

  def iteratorPartialCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: PartialOrder[A]): Double = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next
          val y = ys.next
          val cmp = ev.partialCompare(x, y)
          if (cmp != 0.0) return cmp
        } else {
          return 1.0
        }
      } else {
        return if (ys.hasNext) -1.0 else 0.0
      }
    }
    0.0
  }

  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Eq[A]): Boolean = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          if (ev.neqv(xs.next, ys.next)) return false
        } else {
          return false
        }
      } else {
        return !ys.hasNext
      }
    }
    true
  }
  // scalastyle:on return
}
