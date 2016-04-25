package cats.kernel
package std

import cats.kernel.std.util.StaticMethods.addMap

package object map extends MapInstances

trait MapInstances {
  implicit def mapEq[K, V: Eq]: Eq[Map[K, V]] =
    new MapEq[K, V]
  implicit def mapMonoid[K, V: Semigroup]: MapMonoid[K, V] =
    new MapMonoid[K, V]
}

class MapEq[K, V](implicit V: Eq[V]) extends Eq[Map[K, V]] {
  def eqv(x: Map[K, V], y: Map[K, V]): Boolean =
    x.size == y.size && x.forall { case (k, v1) =>
      y.get(k) match {
        case Some(v2) => V.eqv(v1, v2)
        case None => false
      }
    }
}

class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]]  {
  def empty: Map[K, V] = Map.empty

  def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    addMap(x, y)(V.combine)
}
