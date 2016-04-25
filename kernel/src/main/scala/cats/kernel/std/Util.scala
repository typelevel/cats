package cats.kernel
package std.util

import scala.collection.mutable

object StaticMethods {

  def initMutableMap[K, V](m: Map[K, V]): mutable.Map[K, V] = {
    val result = mutable.Map.empty[K, V]
    m.foreach { case (k, v) => result(k) = v }
    result
  }

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  private[kernel] class WrappedMutableMap[K, V](m: mutable.Map[K, V]) extends Map[K, V] {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
    def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
    def -(key: K): Map[K, V] = m.toMap - key
  }

  def addMap[K, V](x: Map[K, V], y: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    val (small, big, g) =
      if (x.size <= y.size) (x, y, f)
      else (y, x, (v1: V, v2: V) => f(v2, v1))

    val m = initMutableMap(big)
    small.foreach { case (k, v1) =>
      m(k) = m.get(k) match {
        case Some(v2) => g(v1, v2)
        case None => v1
      }
    }
    wrapMutableMap(m)
  }

  def subtractMap[K, V](x: Map[K, V], y: Map[K, V])(subtract: (V, V) => V)(negate: V => V): Map[K, V] = {
    // even if x is smaller, we'd need to call map/foreach on y to
    // negate all its values, so this is just as fast or faster.
    val m = initMutableMap(x)
    y.foreach { case (k, v2) =>
      m(k) = m.get(k) match {
        case Some(v1) => subtract(v1, v2)
        case None => negate(v2)
      }
    }
    wrapMutableMap(m)
  }

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
          if (ev.eqv(xs.next, ys.next)) return true
        } else {
          return false
        }
      } else {
        return !ys.hasNext
      }
    }
    true
  }
}
