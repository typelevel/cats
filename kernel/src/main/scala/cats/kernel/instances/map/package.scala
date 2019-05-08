package cats.kernel
package instances

import scala.collection.mutable

package object map extends MapInstances

trait MapInstances extends MapInstances1 {
  implicit def catsKernelStdHashForMap[K: Hash, V: Hash]: Hash[Map[K, V]] =
    new MapHash[K, V]

  implicit def catsKernelStdCommutativeMonoidForMap[K, V: CommutativeSemigroup]: CommutativeMonoid[Map[K, V]] =
    new MapMonoid[K, V] with CommutativeMonoid[Map[K, V]]
}

trait MapInstances1 {
  implicit def catsKernelStdEqForMap[K, V: Eq]: Eq[Map[K, V]] =
    new MapEq[K, V]
  implicit def catsKernelStdMonoidForMap[K, V: Semigroup]: Monoid[Map[K, V]] =
    new MapMonoid[K, V]
}

class MapHash[K, V](implicit V: Hash[V]) extends MapEq[K, V]()(V) with Hash[Map[K, V]] {
  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3._
  def hash(x: Map[K, V]): Int = {
    var a, b, n = 0
    var c = 1
    x.foreach {
      case (k, v) =>
        // use the default hash on keys because that's what Scala's Map does
        val h = StaticMethods.product2HashWithPrefix(k.hashCode(), V.hash(v), "Tuple2")
        a += h
        b ^= h
        c = StaticMethods.updateUnorderedHashC(c, h)
        n += 1
    }
    var h = mapSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
}

class MapEq[K, V](implicit V: Eq[V]) extends Eq[Map[K, V]] {
  def eqv(x: Map[K, V], y: Map[K, V]): Boolean =
    if (x eq y) true
    else
      x.size == y.size && x.forall {
        case (k, v1) =>
          y.get(k) match {
            case Some(v2) => V.eqv(v1, v2)
            case None     => false
          }
      }
}

class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]] {

  def empty: Map[K, V] = Map.empty

  def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
          my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }

  override def combineAll(xss: TraversableOnce[Map[K, V]]): Map[K, V] = {
    val acc = mutable.Map.empty[K, V]
    xss.foreach { m =>
      val it = m.iterator
      while (it.hasNext) {
        val (k, v) = it.next
        acc(k) = Semigroup.maybeCombine(acc.get(k), v)
      }
    }
    StaticMethods.wrapMutableMap(acc)
  }
}
