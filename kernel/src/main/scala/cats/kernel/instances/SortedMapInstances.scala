package cats.kernel
package instances

import scala.collection.immutable.SortedMap

trait SortedMapInstances extends SortedMapInstances2 {
  implicit def catsKernelStdHashForSortedMap[K: Hash, V: Hash]: Hash[SortedMap[K, V]] =
    new SortedMapHash[K, V]

  @deprecated("Use catsKernelStdHashForSortedMap override without Order", "2.2.0-M3")
  def catsKernelStdHashForSortedMap[K, V](hashK: Hash[K], orderK: Order[K], hashV: Hash[V]): Hash[SortedMap[K, V]] =
    new SortedMapHash[K, V]()(hashV, hashK)

  implicit def catsKernelStdCommutativeSemigroupForSortedMap[K, V: CommutativeSemigroup]
    : CommutativeSemigroup[SortedMap[K, V]] =
    new SortedMapCommutativeSemigroup[K, V]

  implicit def catsKernelStdCommutativeMonoidForSortedMap[K: Order, V: CommutativeSemigroup]
    : CommutativeMonoid[SortedMap[K, V]] =
    new SortedMapCommutativeMonoid[K, V]
}

private[instances] trait SortedMapInstances1 {
  implicit def catsKernelStdEqForSortedMap[K, V: Eq]: Eq[SortedMap[K, V]] =
    new SortedMapEq[K, V]

  @deprecated("Use catsKernelStdEqForSortedMap override without Order", "2.2.0-M3")
  def catsKernelStdEqForSortedMap[K, V](orderK: Order[K], eqV: Eq[V]): Eq[SortedMap[K, V]] =
    new SortedMapEq[K, V]()(eqV)
}

private[instances] trait SortedMapInstances2 extends SortedMapInstances1 {
  implicit def catsKernelStdSemigroupForSortedMap[K, V: Semigroup]: Semigroup[SortedMap[K, V]] =
    new SortedMapSemigroup[K, V]

  implicit def catsKernelStdMonoidForSortedMap[K: Order, V: Semigroup]: Monoid[SortedMap[K, V]] =
    new SortedMapMonoid[K, V]
}

class SortedMapHash[K, V](implicit V: Hash[V], K: Hash[K]) extends SortedMapEq[K, V]()(V) with Hash[SortedMap[K, V]] {

  @deprecated("Use the constructor _without_ Order instead, since Order is not required", "2.2.0-M3")
  private[instances] def this(V: Hash[V], O: Order[K], K: Hash[K]) = this()(V, K)

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3._
  def hash(x: SortedMap[K, V]): Int = {
    var a, b, n = 0
    var c = 1
    x.foreach { case (k, v) =>
      val h = StaticMethods.product2HashWithPrefix(K.hash(k), V.hash(v), "Tuple2")
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

class SortedMapEq[K, V](implicit V: Eq[V]) extends Eq[SortedMap[K, V]] {

  @deprecated("Use the constructor _without_ Order instead, since Order is not required", "2.2.0")
  private[instances] def this(V: Eq[V], O: Order[K]) = this()(V)

  def eqv(x: SortedMap[K, V], y: SortedMap[K, V]): Boolean =
    if (x eq y) true
    else
      x.size == y.size && x.forall { case (k, v1) =>
        y.get(k) match {
          case Some(v2) => V.eqv(v1, v2)
          case None     => false
        }
      }
}

class SortedMapCommutativeSemigroup[K, V](implicit V: CommutativeSemigroup[V])
    extends SortedMapSemigroup[K, V]
    with CommutativeSemigroup[SortedMap[K, V]]

class SortedMapCommutativeMonoid[K, V](implicit V: CommutativeSemigroup[V], O: Order[K])
    extends SortedMapMonoid[K, V]
    with CommutativeMonoid[SortedMap[K, V]]

class SortedMapSemigroup[K, V](implicit V: Semigroup[V]) extends Semigroup[SortedMap[K, V]] {

  def combine(xs: SortedMap[K, V], ys: SortedMap[K, V]): SortedMap[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) { case (my, (k, x)) =>
        my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) { case (mx, (k, y)) =>
        mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }
}

class SortedMapMonoid[K, V](implicit V: Semigroup[V], O: Order[K])
    extends SortedMapSemigroup[K, V]
    with Monoid[SortedMap[K, V]] {

  def empty: SortedMap[K, V] = SortedMap.empty(O.toOrdering)
}
