package cats.kernel
package instances

import scala.collection.mutable

package object map extends MapInstances

trait MapInstances {
  implicit def catsKernelStdEqForMap[K, V: Eq]: Eq[Map[K, V]] =
    new MapEq[K, V]
  implicit def catsKernelStdMonoidForMap[K, V: Semigroup]: Monoid[Map[K, V]] =
    new MapMonoid[K, V]
}

class MapEq[K, V](implicit V: Eq[V]) extends Eq[Map[K, V]] {
  def eqv(x: Map[K, V], y: Map[K, V]): Boolean =
    if (x eq y) true
    else x.size == y.size && x.forall { case (k, v1) =>
      y.get(k) match {
        case Some(v2) => V.eqv(v1, v2)
        case None => false
      }
    }
}

class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]]  {

  def empty: Map[K, V] = Map.empty

  def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) { case (my, (k, x)) =>
        my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) { case (mx, (k, y)) =>
        mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }

  override def combineAll(xss: TraversableOnce[Map[K, V]]): Map[K, V] = {
    val acc = mutable.Map.empty[K, V]
    xss.foreach { m =>
      val it = m.iterator
      while (it.hasNext) {
        val (k, v) = it.next
        m.updated(k, Semigroup.maybeCombine(m.get(k), v))
      }
    }
    StaticMethods.wrapMutableMap(acc)
  }
}
